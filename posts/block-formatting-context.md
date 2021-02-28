---
title: CSS | Block Formatting Context
---

:a[https://www.w3.org/TR/CSS22/visuren.html#block-formatting]{href=https://www.w3.org/TR/CSS22/visuren.html#block-formatting .nav}

> Floats, absolutely positioned elements, block containers (such as inline-blocks, table-cells, and table-captions) that are not block boxes, and block boxes with 'overflow' other than 'visible' (except when that value has been propagated to the viewport) establish new block formatting contexts for their contents. \
> In a block formatting context, boxes are laid out one after the other, vertically, beginning at the top of a containing block. The vertical distance between two sibling boxes is determined by the 'margin' properties. Vertical margins between adjacent block-level boxes in a block formatting context collapse. \
> In a block formatting context, each box's left outer edge touches the left edge of the containing block (for right-to-left formatting, right edges touch). This is true even in the presence of floats (although a box's line boxes may shrink due to the floats), unless the box establishes a new block formatting context (in which case the box itself may become narrower due to the floats).

---

自适应两列布局

```html
<div style="width: 200px;">
    <aside style="background-color: #ffa5a5; float: left; width: 100px; height: 50px;"></aside>
    <main style="background-color: #ffe1c6">
        因为这行文字很长，所以会换行换行换行换行换行换行
    </main>
</div>
```

可以看到右侧元素的一部分跑到了左侧元素下方

因为 BFC 的区域是独立的，不会与页面其他元素相互影响，且不会与 `float` 元素重叠，
那么可以为 `container` 设置 `overflow: hidden;` 触发 `container` 元素的 BFC, 因此就可以形成两列自适应布局

---

清除浮动

```html
<div style="background-color: #ffe1c6; width: 400px;">
    <div style="float: left; width: 200px; height: 200px; background-color: #ffa5a5;"></div>
</div>
```

BFC 计算高度时会计算 `float` 的元素的高度，那么可以为 `container` 设置 `overflow: hidden;` 触发 `container` 元素的 BFC, 因此就可以达到清除浮动影响的效果。

---

防止垂直 `margin` 合并

```html
<div style="background-color: #ffe1c6; margin-bottom: 100px; height: 200px; width: 200px;"></div>
<div style="background-color: #ffe1c6; margin-top: 100px; height: 200px; width: 200px;"></div>
```

中间应有 `200px` 的 `margin` 值，但是却只能看到 `100px`。这是因为他们的外边距相遇发生了合并

因为 BFC 内部是一个独立的容器，所以不会与外部相互影响，可以防止 `margin` 合并。那么可以在中间加入一个元素。并为这个外层元素设置 `overflow: hidden;`，使其形成 BFC