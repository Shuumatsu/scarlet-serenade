---
title: Algorithms & Data Structures | Balanced Binary Search Tree
---

## AVL Tree 

### Insertion

感觉上来说，一次右旋操作会让（新根）左子树的高度减小1，右子树高度不变或加一，本来左 - 右 = 2 不平衡现在变成 （左 - 1） - （右 + 1） = 0 或  （左 - 1） - （右 + 1） = 1

左旋是右旋的对称操作
- right_rotate(left_rotate(root)) = root
- left_rotate(right_rotate(root)) = root

当我们插入某个数字后，可能造成某个节点的左右不平衡，按照插入路径，从底到高处理不平衡的节点。

假设 A 是当前需要处理的节点，因为对称性，不失一般性的，我们假设A的左子树 D 的高度等于右子树 E 高度加 2
