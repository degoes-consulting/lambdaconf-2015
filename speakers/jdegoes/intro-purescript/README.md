# Introduction to Functional Programming with PureScript

This is the repository for the workshop *Introduction to Functional Programming with PureScript*, at LambdaConf 2015 in Boulder, Colorado.

Here, you will find [slides](presentation.md), [source code](src/), and instructions on preparing your computer for the workshop.

# Preparation

First, you should download the contents of this repository to your computer. Then proceed to the following steps.

## 1. Install the PureScript Compiler
  
You should install the [0.6.9.5 release](https://github.com/purescript/purescript/releases/tag/v0.6.9.5). 

## 2. Make sure `psc` and `psc-make` are on your PATH

You can do this by running `psc` from the command-line. If you get an error, you will need to add the location of these tools to your `PATH` environment variable.

## 3. Install Node

<https://nodejs.org/download/>

## 4. Install Bower &amp; Gulp

Bower is used for managing dependencies for most PureScript projects, and Gulp is used to build the project.

```bash
npm install -g bower
npm install -g gulp
```

## 5. Install NPM Dependencies

This installs the tools required to build the project specified in `package.json`.

```bash
npm install
```

## 6. Install Bower Dependencies

This installs the PureScript dependencies specified in `bower.json`.

```bash
bower install
```

## 7. Compile with PureScript

This runs the build process, which will generate a `psc.js` in the `output` directory.

```bash
gulp
```

## 8. Open HTML to Run Game

On Mac OS X:

```
open output/game.html
```

On other platforms, browse the `output` folder and manually open the file `game.html`.