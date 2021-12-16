const Enzyme = require('enzyme')
const Adapter = require('@wojtekmaj/enzyme-adapter-react-17')

exports.configure_ = () => {
  Enzyme.configure({ adapter: new Adapter() })
}

exports.mount_ = Enzyme.mount

exports.shallow_ = Enzyme.shallow

exports.at_ = (index, wrapper) => wrapper.at(index)

exports.debug_ = (wrapper) => wrapper.debug()

exports.exists_ = (selector, wrapper) => wrapper.exists(selector)

exports.find_ = (selector, wrapper) => wrapper.find(selector)

exports.parent_ = wrapper => wrapper.parent()

exports.is_ = (selector, wrapper) => wrapper.is(selector)

exports.length = wrapper => wrapper.length

exports.prop_ = (key, wrapper) => wrapper.prop(key)

exports.setState_ = (newState, wrapper) => {
  return (_onError, onSuccess) => {
    wrapper.setState(newState, onSuccess)

    return (_cancelError, _onCancelerError, onCancelerSuccess) => {
      onCancelerSuccess()
    }
  }
}

exports.simulate_ = (eventType, event, wrapper) => wrapper.simulate(eventType, event)

exports.simulateCustom_ = (eventType, event, wrapper) => wrapper.getElement().props[eventType](event)

exports.state_ = (wrapper) => wrapper.state()

exports.text_ = (wrapper) => wrapper.text()

exports.name_ = (wrapper) => wrapper.name()

exports.unmount_ = (wrapper) => wrapper.unmount()

exports.update_ = (wrapper) => wrapper.update()
