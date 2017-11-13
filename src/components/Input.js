import React, { PureComponent } from 'react'
import styled from 'style-components'

export default class extends PureComponent {

    render() {
        return (
            <div class="centered">
                <div class="content">
                    <div class="sml-10">
                        <div class="field text-field">
                            <label class="">username</label>
                            <div class="input">
                                <input type="text" name="" placeholder="" value="" />
                            </div>
                        </div>
                    </div>
                </div>
            </div>
        )
    }
}