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
        "flag"
        "fmt"
        "log"
        "os"
        "io"
        "os/exec"
        "strings"
        "regexp"

        "github.com/pelletier/go-toml/v2"
)

// stringify nested paths from INPUT as keys in RESULTS (merges/overrides matches)
func flattenMap (input map[string]interface{}, namespace string, results map[string]string) {
        if namespace != "" {
                namespace = namespace + "."
        }

        for key, value := range input {
                nested, is_map := value.(map[string]interface{})
                _, is_array := value.([]interface{})
                if is_map {
                        flattenMap(nested, namespace + key, results)
                } else if is_array {
			// do nothing, for now.
			// for index, _ := range arrayVal {
			// 	index_string := fmt.Sprintf("[%i]", index)
			// 	flattenMap(nested, namespace + key + index_string, results)
			// }
                } else {
			// todo: consider printing if an override happens
                        results[namespace + key] = fmt.Sprintf("%v", value)
                }
        }
}

func shConf (command, value string) string {
        if  strings.ContainsRune(command, '%') {
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
func parent (path, delim string) string {
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
func narrowToNamespace(config map[string]string, ns string) {
        ns = ns + "."
        for key, _ := range config {
                if strings.Index(key, ns) != 0 {
                        delete(config, key)
                }
        }
}

func slurp(f string) string {
	bytes, err := os.ReadFile(f)
	if err != nil {
		panic(err)
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

        config := map[string]string{}

	absorbToml := func(tomltext string) {
                var values map[string]interface{}
                err := toml.Unmarshal([]byte(tomltext), &values)
                if err != nil {
                        panic(err)
                }
                flattenMap(values, "", config)
	}

	mapFlag := func(flagInput []string, action func(string)) {
		for _, v := range flagInput {
			action(v)
		}
	}

	mapFlag(tomlFiles,
		func(tomlFile string) {
			absorbToml(slurp(tomlFile))
		})

	mapFlag(tomlText, absorbToml)

	// process our flatmap
	for key, value := range config {
                config[key] = render(config, value, parent(key, "."))
        }

	// I want partial? or maybe just stop trying to mix styles/do it the go way.
	mapFlag(promotions, func(f string) {promoteNamespace(config, f)})

	if narrow != "" {
		narrowToNamespace(config, narrow)
	}

	switch action {
	case "toml" :
		b, err := toml.Marshal(config)
		if err != nil {
			panic(err)
		}
		fmt.Print(string(b))
	case "shell":
		log.Fatal("shell output not implemented yet.")
	}

	for _, file := range renderTargets {
		fmt.Println(mustache(config, slurp(file)))
	}

	if queryString != "" {
		queryString =  fmt.Sprintf("@{%s}", queryString)
		fmt.Println(render(config, queryString, ""))
	}
}
