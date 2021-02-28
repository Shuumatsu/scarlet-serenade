import React from 'react'

const Indent = props => (
    <>
        <div {...props}></div>
        <style jsx>{`
            div {
                margin-left: 2em;
            }
        `}</style>
    </>
)

export default Indent
