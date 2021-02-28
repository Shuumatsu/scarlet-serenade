---
title: Algorithms & Data Structures | Tree Traversal
---

### :a[94. Binary Tree Inorder Traversal]{href=https://leetcode.com/problems/binary-tree-inorder-traversal/}


#### Recursive
```c++
class Solution {
   public:
    vector<int> inorderTraversal(TreeNode* root) {
        vector<int> ret;
        function<void(TreeNode*)> f = [&](TreeNode* node) {
            if (node == NULL) { return; }

            f(node->left);
            ret.push_back(node->val);
            f(node->right);
        };
        f(root);

        return ret;
    }
};
```

#### Iterative

```c++
class Solution {
   public:
    vector<int> inorderTraversal(TreeNode* root) {
        vector<int> ret;

        // the first node popped from stack will always be the one 
        //     - with no left children
        //     - with all its left children visited
        vector<TreeNode*> stack;

        function<void(TreeNode*)> push = [&](TreeNode* node) {
            TreeNode* curr = node;
            while (curr != NULL) {
                stack.push_back(curr);
                curr = curr->left;
            }
        };

        push(root);
        while (stack.size()) {
            TreeNode* curr = stack[stack.size() - 1];
            stack.pop_back();

            ret.push_back(curr->val);
            push(curr->right);
        }

        return ret;
    }
};
```

### :a[144. Binary Tree Preorder Traversal]{href=https://leetcode.com/problems/binary-tree-preorder-traversal/}

#### Recursive

```c++
class Solution {
   public:
    vector<int> preorderTraversal(TreeNode* root) {
        vector<int> ret;
        function<void(TreeNode*)> f = [&](TreeNode* node) {
            if (node == NULL) { return; }

            ret.push_back(node->val);
            f(node->left);
            f(node->right);
        };
        f(root);

        return ret;
    }
};
```

#### Iterative

```c++
class Solution {
   public:
    vector<int> preorderTraversal(TreeNode* root) {
        vector<int> ret;

        vector<TreeNode*> stack = {root};
        while (stack.size() > 0) {
            TreeNode* curr = stack[stack.size() - 1];
            stack.pop_back();
            if (curr == NULL) { continue; }

            ret.push_back(curr->val);
            stack.push_back(curr->right);
            stack.push_back(curr->left);
        }

        return ret;
    }
};
```

### :a[145. Binary Tree Postorder Traversal]{href=https://leetcode.com/problems/binary-tree-postorder-traversal/}

#### Recursive

```c++
class Solution {
   public:
    vector<int> postorderTraversal(TreeNode* root) {
        vector<int> ret;
        function<void(TreeNode*)> f = [&](TreeNode* node) {
            if (node == NULL) { return; }

            f(node->left);
            f(node->right);
            ret.push_back(node->val);
        };
        f(root);

        return ret;
    }
};
```

#### Iterative

```c++
class Solution {
   public:
    vector<int> postorderTraversal(TreeNode* root) {
        vector<TreeNode*> arr;

        vector<TreeNode*> stack;

        function<void(TreeNode*)> push_left = [&](TreeNode* node) {
            TreeNode* curr = node;
            while (curr != NULL) {
                stack.push_back(curr);
                curr = curr->left;
            }
        };
        if (root != NULL) { push_left(root); }

        while (stack.size() > 0) {
            // the first node popped from stack will always be the one 
            //     - with no left children
            //     - with all its left children visited
            TreeNode* curr = stack[stack.size() - 1];
            stack.pop_back();

            // if `curr->right` exists, in postorder traversal, the last output should be `curr->right`
            // we can use this property to check if the right children have been visited
            if (curr->right == NULL ||
                (arr.size() > 0 && arr[arr.size() - 1] == curr->right)) {
                arr.push_back(curr);
            } else {
                stack.push_back(curr);
                push_left(curr->right);
            }
        }

        vector<int> ret;
        for (TreeNode* node : arr) { ret.push_back(node->val); }
        return ret;
    }
};
```

### :a[105. Construct Binary Tree from Preorder and Inorder Traversal]{href=https://leetcode.com/problems/construct-binary-tree-from-preorder-and-inorder-traversal/}

#### Recursive 

```c++
class Solution {
   public:
    TreeNode* buildTree(vector<int>& preorder, vector<int>& inorder) {
        unordered_map<int, int> mi;
        for (int i = 0; i < inorder.size(); i += 1) {
            mi.insert(make_pair(inorder[i], i));
        }

        function<TreeNode*(int, int, int, int)> f = [&](int ii, int ij, int pi,
                                                        int pj) {
            assert(ij - ii == pj - pi);
            if (ii > ij) { return (TreeNode*)NULL; }

            TreeNode* node = new TreeNode(preorder[pi]);

            int pri = mi.find(node->val)->second;
            int lchildren_len = pri - ii;

            node->left =
                f(ii, ii + lchildren_len - 1, pi + 1, pi + lchildren_len);
            node->right =
                f(ii + lchildren_len + 1, ij, pi + lchildren_len + 1, pj);

            return node;
        };

        return f(0, inorder.size() - 1, 0, preorder.size() - 1);
    }
};
```

### :a[106. Construct Binary Tree from Inorder and Postorder Traversal]{href=https://leetcode.com/problems/construct-binary-tree-from-inorder-and-postorder-traversal/}

#### Recursive 

```c++
class Solution {
   public:
    TreeNode* buildTree(vector<int>& inorder, vector<int>& postorder) {
        unordered_map<int, int> mi;
        for (int i = 0; i < inorder.size(); i += 1) {
            mi.insert(make_pair(inorder[i], i));
        }

        function<TreeNode*(int, int, int, int)> f = [&](int ii, int ij, int pi,
                                                        int pj) {
            assert(ij - ii == pj - pi);
            if (ii > ij) { return (TreeNode*)NULL; }

            TreeNode* node = new TreeNode(postorder[pj]);

            int pri = mi.find(node->val)->second;
            int lchildren_len = pri - ii;

            node->left =
                f(ii, ii + lchildren_len - 1, pi, pi + lchildren_len - 1);
            node->right =
                f(ii + lchildren_len + 1, ij, pi + lchildren_len, pj - 1);

            return node;
        };

        return f(0, inorder.size() - 1, 0, postorder.size() - 1);
    }
};
```

#### Iterative

```c++
class Solution {
   public:
    TreeNode* buildTree(vector<int>& inorder, vector<int>& postorder) {
        assert(inorder.size() == postorder.size());
        if (inorder.size() == 0) { return (TreeNode*)NULL; }

        unordered_map<int, int> mappings;
        for (int i = 0; i < inorder.size(); i += 1) {
            mappings[inorder[i]] = i;
        }

        TreeNode* root = new TreeNode(postorder[postorder.size() - 1]);

        vector<TreeNode*> stack = {root};
        for (int i = postorder.size() - 2; i >= 0; i -= 1) {
            TreeNode* node = new TreeNode(postorder[i]);

            assert(stack.size() > 0);
            TreeNode* parent = stack.back();
            while (stack.size() > 0 &&
                   mappings[node->val] < mappings[stack.back()->val]) {
                parent = stack.back();
                stack.pop_back();
            }

            if (mappings[node->val] < mappings[parent->val]) {
                // left child
                parent->left = node;
            } else {
                // right child
                parent->right = node;
            }

            stack.push_back(node);
        }

        return root;
    }
};
```

### :a[889. Construct Binary Tree from Preorder and Postorder Traversal]{href=https://leetcode.com/problems/construct-binary-tree-from-preorder-and-postorder-traversal}

#### Recursive 

```c++
class Solution {
   public:
    TreeNode* constructFromPrePost(vector<int>& pre, vector<int>& post) {
        unordered_map<int, int> mappings;
        for (int i = 0; i < post.size(); i += 1) { mappings[post[i]] = i; }

        function<TreeNode*(int, int, int, int)> f =
            [&](int pre_i, int pre_j, int post_i, int post_j) {
                assert(pre_j - pre_i == post_j - post_i);
                if (pre_i > pre_j) { return (TreeNode*)NULL; }

                TreeNode* root = new TreeNode(pre[pre_i]);
                if (pre_i == pre_j) { return root; }

                int post_p = mappings[pre[pre_i + 1]];
                int len_left = post_p - post_i + 1;
                root->left =
                    f(pre_i + 1, len_left + (pre_i + 1) - 1, post_i, post_p);
                root->right =
                    f(len_left + (pre_i + 1), pre_j, post_p + 1, post_j - 1);

                return root;
            };

        return f(0, pre.size() - 1, 0, post.size() - 1);
    }
};
```


### :a[1008. Construct Binary Search Tree from Preorder Traversal]{href=https://leetcode.com/problems/construct-binary-search-tree-from-preorder-traversal/}

#### Recursive

```c++
class Solution {
   public:
    TreeNode* bstFromPreorder(vector<int>& preorder) {
        if (preorder.size() == 0) { return NULL; }

        function<TreeNode*(int, int)> f = [&](int i, int j) {
            if (i > j) { return (TreeNode*)NULL; }
            TreeNode* root = new TreeNode(preorder[i]);

            int r = i + 1;
            while (r <= j && preorder[r] < root->val) { r += 1; }
            
            root->left = f(i + 1, r - 1);
            root->right = f(r, j);

            return root;
        };

        return f(0, preorder.size() - 1);
    } 
};
```

we need `O(n)` to get the next greater position `r`, and this can be optimized using monotonic stack 

```c++
class Solution {
   public:
    TreeNode* bstFromPreorder(vector<int>& preorder) {
        if (preorder.size() == 0) { return NULL; }

        vector<int> next_greater(preorder.size(), preorder.size());

        vector<tuple<int, int>> monotonic_decreasing_stack;
        for (int i = 0; i < preorder.size(); i += 1) {
            while (!monotonic_decreasing_stack.empty() &&
                   get<0>(monotonic_decreasing_stack.back()) < preorder[i]) {
                tuple<int, int> p = monotonic_decreasing_stack.back();
                monotonic_decreasing_stack.pop_back();
                next_greater[get<1>(p)] = i;
            }
            monotonic_decreasing_stack.push_back({preorder[i], i});
        }

        function<TreeNode*(int, int)> f = [&](int i, int j) {
            if (i > j) { return (TreeNode*)NULL; }
            TreeNode* root = new TreeNode(preorder[i]);

            root->left = f(i + 1, min(j, next_greater[i] - 1));
            root->right = f(next_greater[i], j);

            return root;
        };

        return f(0, preorder.size() - 1);
    }
};
```

---

can be further optimized into a 1-pass solution, 与其根据 `preorder[i]` 判断子树的范围，我们可以根据 `lower_bound` 与 `upper_bound` 来确定 `preorder[curr]` 是否属于当前子树

```c++
class Solution {
   public:
    TreeNode* bstFromPreorder(vector<int>& preorder) {
        int curr = 0;
        // 根据 `lower_bound` 与 `upper_bound` 返回从 `curr` 开始的子树
        function<TreeNode*(int, int)> f = [&](int lower_bound,
                                              int upper_bound) {
            // `curr` not within bound, 说明从 `curr` 开始的元素不在当前子树中
            if (curr == preorder.size() || preorder[curr] <= lower_bound ||
                preorder[curr] >= upper_bound) {
                return (TreeNode*)NULL;
            }

            TreeNode* node = new TreeNode(preorder[curr]);
            curr += 1;

            node->left = f(lower_bound, node->val);
            node->right = f(node->val, upper_bound);

            return node;
        };

        return f(INT32_MIN, INT32_MAX);
    }
};
```

#### Iterative

首先注意几个 preorder traversal 的性质：
- if `to_node(preorder[i])->left != NULL` then `to_node(preorder[i])->left == to_node(preorder[i + 1])` 
- when visiting some node, all its children should not have been visited
- when visiting `right_child` 
    - parent and all parent's left children should have been visited
    - `parent->val` is the greatest value less than `right_child->val`


```c++
class Solution {
   public:
    TreeNode* bstFromPreorder(vector<int>& preorder) {
        if (preorder.size() == 0) { return NULL; }

        TreeNode* root = new TreeNode(preorder[0]);
        
        // 如果我们用一个 monotonic decreasing stack，按 `preorder[].map(to_node)` 压入，那么可以保证
        // - 如果栈顶结点的值大于当前节点值，那么栈顶结点是当前节点的 parent: `parent->left = node`
        // - 自栈顶开始，最大的小于当前节点值的节点是当前节点的 parent：`parent->right = node`
        vector<TreeNode*> stack = {root};
        for (int i = 1; i < preorder.size(); i += 1) {
            TreeNode* node = new TreeNode(preorder[i]);

            TreeNode* parent = stack.back();
            while (stack.size() > 0 &&
                   stack[stack.size() - 1]->val < node->val) {
                parent = stack.back();
                stack.pop_back();
            }

            if (node->val < parent->val) {
                parent->left = node;
            } else {
                parent->right = node;
            }

            stack.push_back(node);
        }

        return root;
    }
};
```

### Construct binary tree from preorder and inorder traversal

Let's use here two facts:
- Binary tree could be constructed from preorder and inorder traversal.
- Inorder traversal of BST is an array sorted in the ascending order.