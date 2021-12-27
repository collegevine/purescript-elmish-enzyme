exports.adapter = (pkg) => () => {
  const Adapter = require(pkg)
  return new Adapter()
}
