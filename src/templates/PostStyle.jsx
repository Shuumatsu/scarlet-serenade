import { createGlobalStyle, css, keyframes } from 'styled-components'
import plate from '../styles/plate'

const emphasize = color => css`
    color: ${color};
    position: relative;
    padding-bottom: 2px;
    &:after {
        content: '';
        height: 2px;
        background: ${color};
        width: 0;
        position: absolute;
        bottom: 0;
        left: 0;
    }
    &:hover {
        &:after {
            width: 100%;
            animation-duration: 0.8s;
            animation-name: ${animation};
            animation-iteration-count: 1;
        }
    }
`

const animation = keyframes`
    0% {
        left: 50%;
        width: 10%;
    }

    100% {
        left: 0;
        width: 100%;
    }
`

const AppStyle = createGlobalStyle`
    a {
        white-space: nowrap;
        overflow: hidden;
        text-overflow: ellipsis;

        text-decoration: none;
        color: ${plate['text-color']};
    }

    hr {
        border: 0;

        height: 1px;
        width: calc(100% / 3);

        margin-left: auto;
        margin-right: auto;

        background-image: -webkit-linear-gradient(left, #f0f0f0, #8c8b8b, #f0f0f0);
        background-image: -moz-linear-gradient(left, #f0f0f0, #8c8b8b, #f0f0f0);
        background-image: -ms-linear-gradient(left, #f0f0f0, #8c8b8b, #f0f0f0);
        background-image: -o-linear-gradient(left, #f0f0f0, #8c8b8b, #f0f0f0); 

    }

    .nav {
        color: ${plate['text-color-light']};
        border-bottom: 1px solid ${plate['green-color']};
        box-shadow: inset 0 -7px 0 ${plate['green-color']};
        transition: all 0.25s ease-in-out;
        padding: 2px 2px 0 2px;
        &:hover {
            background-color: ${plate['green-color']};
        }
    }

    .definition {
        ${emphasize(plate['purple-color'])}
    }

    .assumption {
        ${emphasize(plate['primary-color'])}
    }

    .emphasize {
        ${emphasize(plate['blue-color-light'])}
    }
`

export default AppStyle
