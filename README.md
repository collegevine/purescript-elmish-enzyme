# purescript-elmish-enzyme

`elmish-enzyme` is a testing library for Elmish which provides bindings for [Enzyme](https://enzymejs.github.io/enzyme/), a React testing library. Enzyme allows mounting a component, then interacting with it and testing properties of the component. This library offers a monadic API around Enzyme, which lets you write tests like this:

```purs
import Elmish.Enzyme (testComponent, text, find, at, simulate, (>>))

someTest = it "has two buttons with the right text" do
  testComponent myComponent do
    find ".card" >> at 3 >> find "button" >> simulate "click"
    find ".modal" >> text >>= shouldEqual "Modal content"
```

## Getting Started

To start using this library, you’ll need to install Enzyme and [JSDOM](https://github.com/jsdom/jsdom).

```
npm install enzyme jsdom global-jsdom --save-dev
```

JSDOM is a lightweight library which emulates, to some degree, a web browser. Enzyme relies on it for “[Full DOM Rendering](https://enzymejs.github.io/enzyme/docs/api/mount.html).” As stated in the [Enzyme docs](https://enzymejs.github.io/enzyme/docs/guides/jsdom.html), JSDOM should be configured before React is required. To do this, you can require it in your main test module’s FFI, like so:

```js
// test/Main.js

const jsdom = require('global-jsdom')

jsdom()

exports._configureJsDomViaFfi = null
```

```purs
-- test/Main.purs

foreign import _configureJsDomViaFfi :: Type
```

You’ll also need to install an adapter for whichever version of React you’re using. Install an adapter from the list below, e.g.:

```
npm install enzyme-adapter-react-16 --save-dev
```

Then configure enzyme to use it in your main test module:

```purs
main = do
  Enzyme.configure Adapter.react_16_4
  launchAff_ $ runSpec [specReporter] spec
```

This library supports the following adapters:

| Adapter Name (in Adapter.purs) | Enzyme Adapter Package | React SemVer Compatibility |
| --- | --- | --- |
| `unofficialReact_17` | `@wojtekmaj/enzyme-adapter-react-17` | `^17.0.0` |
| `react_16_4` | `enzyme-adapter-react-16` | `^16.4.0-0` |
| `react_16_3` | `enzyme-adapter-react-16.3` | `~16.3.0-0` |
| `react_16_2` | `enzyme-adapter-react-16.2` | `~16.2` |
| `react_16_1` | `enzyme-adapter-react-16.1` | <code>~16.0.0-0 &#124;&#124; ~16.1</code> |
| `react_15_5` | `enzyme-adapter-react-15` | `^15.5.0` |
| `react_15_4` | `enzyme-adapter-react-15.4` | `15.0.0-0 - 15.4.x` |
| `react_14` | `enzyme-adapter-react-14` | `^0.14.0` |
| `react_13` | `enzyme-adapter-react-13` | `^0.13.0` |

## Examples

Examples of how to write tests can be found in the [test](https://github.com/collegevine/purescript-elmish-enzyme/tree/main/test) folder.
