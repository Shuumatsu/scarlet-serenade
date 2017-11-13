import { isNil, forEach } from 'ramda'

export default attrs => constructor => {

    class decorated extends constructor {
        static get observedAttributes() {
            return attrs
        }
    }

    forEach(attr => Object.defineProperty(decorated.prototype, attr, {
        get: function () {
            return this[attr]
        },
        set: function (val) {
            if (isNil(val))
                this.removeAttribute(attr)
            this.setAttribute('disabled', val)
        }
    }), attrs)

    return decorated
}
