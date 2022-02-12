export function adapterImpl(pkg) {
  return () => {
    const Adapter = import(pkg)
    return Adapter.then(a => new a())
  };
}
