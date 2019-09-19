# ABVDTools
Tools in R to get data and statistics from the ABVD-database

To install use devtools

```R
devtools::install_github("https://github.com/KonstantinHoffmann/ABVDTools")
```

before you can use it, you need to specify a folder for the language data to download. Do this via

```R
options(abvdLanguageFolder = "/path/to/my/folder")
```

ABVDTools will download all necessary language files into this folder.
