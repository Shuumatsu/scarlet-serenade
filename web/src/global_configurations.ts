import css from './app.css'
import normalized from 'normalize.css'
import katex from 'katex/dist/katex.css'

const head = document.querySelector('head')

const normalized_style = document.createElement('style')
normalized_style.innerHTML = normalized
head?.append(normalized_style)

const app_style = document.createElement('style')
app_style.innerHTML = css
head?.append(app_style)

const katex_style = document.createElement('style')
katex_style.innerHTML = katex
head?.append(katex_style)
