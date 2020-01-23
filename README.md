# elm-minesweeper

[Play it here!](https://brandly.github.io/elm-minesweeper/)

![screenshot](src/images/screenshot.png?raw=true)

### development

Set up.

```shell
$ npm install
$ npm start
```

At this point, you could open static files and manually build after changing files.

```shell
$ open dist/index.html
$ npm run build
```

You could also use `elm reactor` to automatically run your build. Just refresh the page after making changes.

```shell
$ npm run reactor
$ open http://localhost:8000/src/Main.elm
```

`elm-format` is used to format the code here.

```shell
$ npm run format
```
