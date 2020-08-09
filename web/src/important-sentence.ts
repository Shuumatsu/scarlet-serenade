import { LitElement, html, css, customElement } from 'lit-element'

@customElement('important-sentence')
export class ImportantSentence extends LitElement {
    static get styles() {
        return [
            css`
                :host > p {
                    border-bottom: 1px solid salmon;
                    box-sizing: border-box;
                }
            `,
        ]
    }

    render() {
        return html`<p><slot></slot></p> `
    }
}
