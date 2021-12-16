package main

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

func flattenMap (input map[string]interface{}, preamble string, results map[string]string) {
        if preamble != "" {
                preamble = preamble + "."
        }

        for key, value := range input {
                nested, is_map := value.(map[string]interface{})
                arrayVal, is_array := value.([]interface{})
                if is_map {
                        flattenMap(nested, preamble + key, results)
                } else if is_array {
                        newVal := ""
                        for _, val := range arrayVal {
                                newVal = newVal + "\n" + fmt.Sprintf("%v", val)
                        }
                        results[preamble + key] = newVal
                } else {
                        results[preamble + key] = fmt.Sprintf("%v", value)
                }
        }
}

func shConf (command, value string) string {
        if  strings.ContainsRune(command, '%') {
                shell := strings.ReplaceAll(command, "%", value)
                out, err := exec.Command("sh", "-c", shell).Output()

                if err != nil {
                        log.Fatal(err)
                }
                return string(out)
        } else {
                cmd := exec.Command("sh", "-c", command)
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

func parent (path, delim string) string {
	parts := strings.Split(path, delim)
	return strings.Join( parts[0:len(parts)-1], delim)
}

func render(config map[string]string, template string, namespace string) string {
        transformPattern := ":[A-Za-z\\.]+"
        referencePattern := fmt.Sprintf("[^@]?@\\{([^{}:]+)(%s)*\\}", transformPattern)
        referenceRe, _ := regexp.Compile(referencePattern)

        matches := referenceRe.FindAllStringSubmatch(template, -1)
        if matches == nil {
                return template
        }

        for _, groups := range matches {
                match := groups[0]
                ident := groups[1]
                transformers := groups[2:]

                var result = ""

                local_ident := namespace + "." + ident
                if _, ok := config[local_ident]; ok {
                        result = render(config, config[local_ident], "")
                } else {
                        result = render(config, config[ident], "")
                }

                for _, transformer := range transformers {
                        if transformer != "" {
                                local_ident := namespace + "." + transformer[1:]
                                // local_ident2 := ident + "." + transformer[1:]

                                if _, ok := config[local_ident]; ok {
                                        local_ident = "@{" + local_ident + "}"
                                        result = shConf(render(config, local_ident, ""), result)
                                } else {
                                        transformer = "@{" + transformer[1:] + "}"
                                        result = shConf(render(config, transformer, ""), result)
                                }

                        }
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


        // nested, ok := tomlFiles.([]string)
        fmt.Println(tomlFiles)
        // println([]string(tomlFiles))

        // os.Exit(0)

        // other thoughts:
        // the panel stuff is insane. need to contextualize env for calling
        // ripen/squeeze from p_format. don't know if we want to get rigid here
        // or have a panel transformer elsewhere.

        // also begs the question about mustach templating//other stuff

        // what do arrays look like? both in mustache and templating

        // honk -l file -l file -l file -t template -r (reload)

        // usage (dynamic/query)

        // honk -l file -l file -l file -v path.to.value (cache by default)
        // honk -l file -l file -l file -E (dump env variables in an eval fashion)
        // -c context (promote a table to top level)

        // tomlFiles := []string {
        //         "/Users/nathan/.dotfiles/wm/.wm/themes/font.toml",
        //                 // "/Users/nathan/.dotfiles/wm/.wm/themes/temp.toml"
        //         }

        config := map[string]string{}

        tomlString := ""
        for _, tomlFile := range tomlFiles {
                tomlBytes, _ := os.ReadFile(tomlFile)
                var values map[string]interface{}

                tomlString = tomlString + "\n" + string(tomlBytes)
                err := toml.Unmarshal(tomlBytes, &values)
                if err != nil {
                        panic(err)
                }
		// nb: this is also a merge
                flattenMap(values, "", config)
        }

        for key, value := range config {
                var parts = strings.Split(key, ".")
		var namespace = strings.Join(parts[0:len(parts)-1], ".")
                config[key] = render(config, value, namespace)
        }

	for _, context := range contexts {
		promoteNamespace(config, context)
	}

	if queryString != "" {
		queryString =  fmt.Sprintf("@{%s}", queryString)
		fmt.Println(render(config, queryString, ""))
	}
}
