import { LitElement, html, customElement, css } from 'lit-element'

@customElement('small-br')
export class SmallBr extends LitElement {
    static get styles() {
        return css`
            div {
                height: 0.5em;
                width: 1px;
            }
        `
    }

    render() {
        return html`
            <div />
        `
    }
}
