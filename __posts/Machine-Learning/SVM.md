考虑一堆不同颜色的水果，我们要根据其颜色推测是否可食用，根据数据集作出下图

<img src="D:\learning-notes\Machine-Learning\image-20200606012155175.png" alt="image-20200606012155175" style="zoom:33%;" />

SVM can allow us to do this。他的工作方式是用形状像公路的线条，分隔两组不同的数据，两边就像公路的两条车道，中心是一条虚线。SVM 会使这两条车道尽量达到最宽，直到边缘两条实线与数据集发生重合。

<img src="D:\learning-notes\Machine-Learning\image-20200606012226349.png" alt="image-20200606012226349" style="zoom:33%;" />

中心的虚线起分类做用，这幅图中处于中心虚线右侧的被认为是不可吃的，左边的被认为是可吃的

但是情况并不总是这么单纯，总会有一些 don't really fall with the group。我们不能把圈和叉严格分开了。
不过我们依然可以画出一条带间隔的分割线，只不过这时每一个落在错误一侧的点都需要根据它本该所在一侧间隔线的距离被罚分。我们要做的是移动分隔线的位置，调整间隔的宽度，使最后的罚分相加所得的值尽量小

<img src="D:\learning-notes\Machine-Learning\image-20200606012321439.png" alt="image-20200606012321439" style="zoom:33%;" />

所以当你的数据点不能完美分开时，依旧可以使用 SVM。这种情况被称为线性不可分

---

更糟糕的情况是，无论我们怎么画线，总会有相当大的部分的点无法被正确分离



<img src="D:\learning-notes\Machine-Learning\image-20200606012042208.png" alt="image-20200606012042208" style="zoom: 33%;" />

SVM 对这种情况也有解决办法，现在设想所有的数据点都在一个平面上，比如说都在一条橡胶垫上，你可以把橡胶垫拿起来，扭曲、折叠、拉伸，想怎么折腾都可以，当扭曲到一定程度的时候，你可以仅用一个平面，把好的和不好的分割开来

<img src="D:\learning-notes\Machine-Learning\image-20200606012832693.png" alt="image-20200606012832693" style="zoom:33%;" />

这种折叠空间的技巧叫做 kernel trick，其处理过程涉及复杂的运算。

## Drawbacks

1. Data with lots of error 
    discrimination location depends entirely on the few nearest data points
    所以这些部分产生的错误会造成非常大的影响
2. Choosing the wrong kernel
    kernel selection 需要很多的试错和丰富的经验
3. Large data sets
    calculating the kernel is expensive

each of these requires human in the loop to make judgement calls