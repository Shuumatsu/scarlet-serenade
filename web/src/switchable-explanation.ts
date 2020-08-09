import { css, customElement, html, LitElement, property, unsafeCSS } from 'lit-element'
import feather from 'feather-icons'
import { unsafeHTML } from 'lit-html/directives/unsafe-html'

const minus = feather.icons['minus'].toSvg({ height: '1em', color: 'salmon' })
const plus = feather.icons['plus'].toSvg({ height: '1em', color: 'salmon' })

@customElement('switchable-explanation')
export class SwitchableExplanation extends LitElement {
    @property({ reflect: true }) active = true
    @property({ reflect: true }) description = ''

    static get styles() {
        return css`
            :host > .description {
                cursor: pointer;
            }
            slot[active='false'] {
                display: none;
            }
        `
    }

    switch = (event: Event) => {
        console.log(event)
        this.active = !this.active
    }

    render() {
        return html`
            <p class="description" @click=${this.switch}>
                ${unsafeHTML(this.active ? minus : plus)}
                <span>${this.description}</span>
            </p>
            <slot active=${this.active}></slot>
        `
    }
}
