package main

// todo:
// "checking" modes/linting modes when rendering
// tests

// some thoughts...
// would a more "golang" way of doing things be to build a structure for our
// string-string map? and then have functions that act on it for getting
// individual values, rendering and such?

// right now this is very dynamic, and the style doesn't feel like it's "jiving"
// (but that could also just be my go comfort level)

import (
	"bytes"
	"crypto/md5"
	"encoding/binary"
	"encoding/gob"
	"flag"
	"fmt"
	"io"
	"log"
	"os"
	"os/exec"
	"regexp"
	"strings"
	"time"

	"github.com/pelletier/go-toml/v2"
)

// stringify nested paths from INPUT as keys in RESULTS (merges/overrides matches)
func flattenMap(input map[string]interface{}, namespace string, results map[string]string) {
	if namespace != "" {
		namespace = namespace + "."
	}

	for key, value := range input {
		nested, is_map := value.(map[string]interface{})
		_, is_array := value.([]interface{})
		if is_map {
			flattenMap(nested, namespace+key, results)
		} else if is_array {
			// do nothing, for now.
			// for index, _ := range arrayVal {
			// 	index_string := fmt.Sprintf("[%i]", index)
			// 	flattenMap(nested, namespace + key + index_string, results)
			// }
		} else {
			// todo: consider printing if an override happens
			results[namespace+key] = fmt.Sprintf("%v", value)
		}
	}
}

func shConf(command, value string) string {
	if strings.ContainsRune(command, '%') {
		shell := strings.ReplaceAll(command, "%", value)
		out, err := exec.Command("bash", "-c", shell).Output()

		if err != nil {
			log.Fatal(err)
		}
		return string(out)
	} else {
		cmd := exec.Command("bash", "-c", command)
		stdin, _ := cmd.StdinPipe()

		go func() {
			defer stdin.Close()
			io.WriteString(stdin, value)
		}()

		out, err := cmd.CombinedOutput()
		if err != nil {
			log.Fatal(command)
			log.Fatal(err)
		}

		return string(out)
	}
}

// Get the parent of a path using delim.
func parent(path, delim string) string {
	parts := strings.Split(path, delim)
	return strings.Join(parts[0:len(parts)-1], delim)
}

// Do a basic mustache template matcher -- only handles direct matches.
func mustache(config map[string]string, template string) string {
	stacheRe := regexp.MustCompile("\\{\\{([^{}]+)\\}\\}")
	matches := stacheRe.FindAllStringSubmatch(template, -1)

	for _, groups := range matches {
		match := groups[0]
		ident := groups[1]

		if result, ok := config[ident]; ok {
			template = strings.ReplaceAll(template, match, result)
		} else {
			println("gott: template key not found!: ", ident)
		}
	}

	return template
}

// Render some string with the templating syntax and a map of strings to strings.
//
// @{path.to.value:_.transformer}
// @{localValue:localTransformer} (context given through namespace)
func render(config map[string]string, template string, namespace string) string {
	transformPattern := ":[A-Za-z\\.-_]+"
	referencePattern := fmt.Sprintf("[^@]?(@\\{([^{}:]+)(%s)*\\})", transformPattern)
	referenceRe := regexp.MustCompile(referencePattern)

	matches := referenceRe.FindAllStringSubmatch(template, -1)
	if matches == nil {
		return template
	}

	for _, groups := range matches {
		match := groups[1]
		ident := groups[2]
		transformers := groups[3:]

		var result = ""

		local_ident := namespace + "." + ident
		if _, ok := config[local_ident]; ok {
			ident = local_ident
		}

		result = render(config, config[ident], parent(ident, "."))

		for _, transformer := range transformers {
			if transformer != "" {
				transformer = transformer[1:]
				local_ident := namespace + "." + transformer

				if _, ok := config[local_ident]; ok {
					transformer = local_ident
				}

				transformer_ns := parent(transformer, ".")
				transformer = "@{" + transformer + "}"
				result = shConf(render(config, transformer, transformer_ns), result)
			}
		}

		result = strings.TrimSpace(result)
		if result == "" {
			println("render resulted in nothing!: ", namespace, match)
		}
		template = strings.ReplaceAll(template, match, result)
	}
	return template
}

// Promote value in config to top level
func promoteNamespace(config map[string]string, ns string) {
	ns = ns + "."
	for key, value := range config {
		if strings.Index(key, ns) == 0 {
			new_key := key[len(ns):len(key)]
			config[new_key] = value
		}
	}
}

// Filter to values starting with ns
func narrowToNamespace(config map[string]string, ns string) map[string]string {
	// it appears mutating iterations are non-deterministic?
	new_config := map[string]string{}

	ns = ns + "."
	for key, value := range config {
		if strings.Index(key, ns) == 0 {
			new_key := key[len(ns):len(key)]
			new_config[new_key] = value
		}
	}
	return new_config
}

func slurp(f string) string {
	bytes, err := os.ReadFile(f)
	if err != nil {
		log.Fatalf("file not found: %s", f)
	}
	return string(bytes)
}

// add an array type for flag
type arrayFlag []string

func (i *arrayFlag) String() string {
	return "nope"
}

func (i *arrayFlag) Set(value string) error {
	*i = append(*i, value)
	return nil
}

func act(config map[string]string, renderTargets []string, action, queryString string) {
	switch action {
	case "toml":
		b, err := toml.Marshal(config)
		if err != nil {
			panic(err)
		}
		fmt.Print(string(b))
	case "keys":
		for k, _ := range config {
			fmt.Println(k)
		}
	case "shell":
		for k, v := range config {
			k = strings.ReplaceAll(k, ".", "_")
			// meh on this replace value
			k = strings.ReplaceAll(k, "-", "_")
			v = strings.ReplaceAll(v, "'", "'\\''")
			fmt.Printf("%s='%s'\n", k, v)
		}
	}

	for _, file := range renderTargets {
		fmt.Println(mustache(config, slurp(file)))
	}

	if queryString != "" {
		if result, ok := config[queryString]; ok {
			fmt.Println(result)
		} else {
			log.Fatal("query not found")
		}
	}
}

func getConfig(tomlFiles, tomlText []string) map[string]string {
	config := map[string]string{}

	sumStrings := append(tomlFiles, tomlText...)
	sumBytes := md5.Sum([]byte(strings.Join(sumStrings, "")))
	sumInt := binary.BigEndian.Uint64(sumBytes[:])
	cache_file := os.Getenv("HOME") + "/.cache/gott/" + fmt.Sprintf("%v", sumInt)
	cached := true

	fetchTime := func(f string) time.Time {
		info, err := os.Stat(f)
		if err != nil {
			log.Fatalf("err when stating file '%s': %s", f, err)
		}
		return info.ModTime()
	}

	cache_bytes, err := os.ReadFile(cache_file)
	if err != nil {
		cached = false
	}

	if cached {
		cache_modTime := fetchTime(cache_file)
		for _, f := range tomlFiles {
			if fetchTime(f).After(cache_modTime) {
				cached = false
			}
		}
	}

	if cached {
		d := gob.NewDecoder(bytes.NewReader(cache_bytes))

		// Decoding the serialized data
		err = d.Decode(&config)
		if err != nil {
			panic(err)
		}
		return config
	}

	// ok, we aren't cached. time to:
	// ａｂｓｏｒｂ
	// ｒｅｎｄｅｒ
	// ｃａｃｈｅ
	for _, file := range tomlFiles {
		// "prepend"
		tomlText = append([]string{slurp(file)}, tomlText...)
	}

	for _, text := range tomlText {
		var values map[string]interface{}
		err := toml.Unmarshal([]byte(text), &values)
		if err != nil {
			log.Fatalf("%s\n-----\nerror parsing: %s")
		}
		flattenMap(values, "", config)
	}

	for key, value := range config {
		config[key] = render(config, value, parent(key, "."))
	}

	b := new(bytes.Buffer)
	e := gob.NewEncoder(b)
	err = e.Encode(config)
	if err != nil {
		panic(err)
	}

	var perm os.FileMode = 0o644
	os.WriteFile(cache_file, b.Bytes(), perm)
	// todo: cache eviction/cleanup

	return config
}

func main() {
	var tomlFiles, promotions, renderTargets, tomlText arrayFlag
	var action, queryString, narrow string

	flag.Var(&tomlFiles, "t", "Add a toml file to consider")
	flag.Var(&tomlText, "T", "Add raw toml to consider")
	flag.Var(&promotions, "p", "Promote a namespace to the top level")
	flag.Var(&renderTargets, "r", "Render a file")
	flag.StringVar(&action, "o", "", "Output type <shell|toml>")
	flag.StringVar(&queryString, "q", "", "Query for a value (implicit surrounding @{})")
	flag.StringVar(&narrow, "n", "", "Narrow the namespaces to consider")

	flag.Parse()

	config := getConfig(tomlFiles, tomlText)

	for _, p := range promotions {
		promoteNamespace(config, p)
	}

	if narrow != "" {
		config = narrowToNamespace(config, narrow)
	}

	act(config, renderTargets, action, queryString)
}
