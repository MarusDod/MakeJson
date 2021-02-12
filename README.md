# MakeJson

Build tool similar to Makefile.

Installation:

Clone project:
>    git clone "https://github.com/MarusDod/MakeJson"
>    cd MakeJson

Install Stack:
>    curl -sSL https://get.haskellstack.org/ | sh
or
>    wget -qO- https://get.haskellstack.org/ | sh

Install executable to path:
>    stack install

If you do not have ghc (haskell compiler) or don't have a lot of the  libs installed this will take a while


Guide:

If you're used to Makefile, this works quite the same

You need to create a file named "config.json" on your current directory and write the targets needed to compile your project.
There are three types of statements: definitions, rule declarations and phonies

example:

```Json
    {
        "declarations" : {
            "$(CC)" : "gcc",
            "$(files)" : ["main.c"],
            "$(includes)" : "header.h",
            "$(target)" : "exe.out"
        },
        "rules" : {
            "$(target)" : {
                "deps" : ["$(files)","$(includes)"],
                "commands" : [
                    ["$(CC)","-o","$(target)","$(files)"]
                ]
            }
        },
        "phonies" : {
            "clean" : {
                "commands" : [
                    ["rm -f",$(target)]
                ]
            }
        }
    }
```

is the equivalent of:

```Makefile
CC=gcc
target=exe.out
files=main.c
includes=include.h

$(target):$(files) $(includes)
    $(cc) -o $(target) $(files)

.phony:clean

clean:
    rm -f $(target)
```

To run:
>    MakeJson-exe <target>
or if you define assign `$(all)` to a variable
>    MakeJson-exe


To uninstall:
>    cd $PATH/MakeJson
>    stack uninstall
>    rm -rf .
