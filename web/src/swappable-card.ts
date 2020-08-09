import { LitElement, html, property, customElement } from 'lit-element'

@customElement('swappable-card')
export class SwappableCard extends LitElement {
    @property() name = 'World'

    render() {
        return html`
            <p>Hello, ${this.name}!</p>
        `
    }
}
