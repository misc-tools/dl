## Jan 17, 2017 

- Start the project. 
- Some libraries that I think should be used in this project: 
  + `tagsoup`: to parse HTML page to find link 
  + `optparse-applicative`: to parse command line argument 
  + `async`: for concurrent downloads. 
  
## Jan 18, 2017 

- To implement: Given a URL, count how many resources of each type. 
- Syntax should be: 

```sh
$ dl --explore --url www.google.com
```

- `optparse-applicative` supports 4 kinds of options: regular options,
  flags, arguments and commands. 
  
- Above, `explore` should be a flag, `url` should be a regular
  option. 
  
- Used tagsoup to get all the links in the webpage and display. 

- Note that some links are absolute link while some are relative: 

```sh
http://panhu.me/pdf/LF.pdf
http://panhu.me/pdf/BST.pdf
./papers/mobicom14_ekhonet.pdf
./papers/nsdi14_quarknet.pdf
```
- Extracted the file types from the link 
- Need to think more about the flow of the program. 

## Jan 19, 2017 

- Tutorial for `optparse-applicative`:
  https://hackage.haskell.org/package/optparse-applicative
  
- For the option `filetype`, I use a regular option. Need a default
  value, default to pdf. 

- I extracted the links of a certain filetype from the website. The
  way I did it is that I get all the links and filter them using
  `takeExtension` function. 
  
- The relative links are normalized to absolute links.   

- From a list of links, I downloaded the files concurrently using
  `async` package.
  
- Currently there Need to handle exceptions carefully 

- To implement:
  + where to save files
  + exception handlers

## Jan 20, 2017 

- Need to save ByteString to file. Functions to manipulate filenames
and directories are in `System.FilePath.Posix`

- Had some problem with https pages in `networ-http`, need to fix
  it. People suggest using `http-conduit`. 
  
- Bug: the function to convert relative path to absolute path is not
  generalized enough! Better use functions from `network-uri`. 
  
- 

