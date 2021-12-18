package main

// todo:
// "checking" modes/linting modes when rendering
// tests

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

func promoteNamespace(config map[string]string, ns string) map[string]string {
        ns = ns + "."
        // promote value in config to top level
        for key, value := range config {
                if strings.Index(key, ns) == 0 {
                        new_key := key[len(ns):len(key)]
                        config[new_key] = value
                }
        }
        return config
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
        var tomlFiles, contexts, renderTargets arrayFlag
	var action, queryString string

        flag.Var(&tomlFiles, "l", "Add a toml file to consider")
        flag.Var(&contexts, "c", "Promote a namespace to the top level")
        flag.Var(&renderTargets, "r", "Render a file")
        flag.StringVar(&action, "o", "", "Output type <shell|toml>")
        flag.StringVar(&queryString, "q", "", "Query for a value (implicit surrounding @{})")

        flag.Parse()

        config := map[string]string{}

        for _, tomlFile := range tomlFiles {
                tomlBytes, _ := os.ReadFile(tomlFile)
                var values map[string]interface{}

                err := toml.Unmarshal(tomlBytes, &values)
                if err != nil {
                        panic(err)
                }
                flattenMap(values, "", config)
        }

	for key, value := range config {
                config[key] = render(config, value, parent(key, "."))
        }

	for _, context := range contexts {
		promoteNamespace(config, context)
	}

	switch action {
	case "toml" :
		log.Fatal("toml output not implemented yet.")
	case "shell":
		log.Fatal("shell output not implemented yet.")
	default:
	}

	// for k, v := range config {
	// 	fmt.Printf("%s: %s\n", k, v)
	// }

	for _, file := range renderTargets {
                fileBytes, _ := os.ReadFile(file)
		fmt.Println(mustache(config, string(fileBytes)))
	}

	if queryString != "" {
		queryString =  fmt.Sprintf("@{%s}", queryString)
		fmt.Println(render(config, queryString, ""))
	}
}
