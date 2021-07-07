---
title: Operating System | File System
---

The implementation is organized in 7 layers

- **file descriptor**: abstracts many unix resources (e.g., pipes, devices, files, etc.) using the file sysrtem interface

- **pathname**: provides hierachical path names like `/workspace/scarlet-serenade`, and resolves them with recursive lookup

- **directory**: implements each directory as a specified kind of inode whose content is a sequence of directory entries, each of which contains a file's name and i-number

- **inode**: provides individual files, each represented with a unique i-number and some blocks holding its data

- **logging**: allows higher layers to wrap updates to several blocks in a transaction, and ensures that blocks are updated atomically in the face of crashes

- **buffer cache**: 
    1. synchronize access to disk blocks to ensure that only one copy of a block is in memory and that only one kernel thread at a time uses that copy; 
    2. cache popular blocks so that they don't need to be re-read from the slow disk. 

- **disk**: read/writes blocks on hardware drive

--- 

```
┌──────┬───────┬─────┬────────┬────────┬─────────┐
│ boot │ super │ log │ inodes │ bitmap │ data... │
└──────┴───────┴─────┴────────┴────────┴─────────┘
```

- Block 0 holds the boot sector
- Block 1 is called the superblock: it contains metadata about the file system 
    - the file system size in blocks
    - the number of data blocks
    - the number of inodes
    - the number of blocks in the log
- Blocks starting at 2 hold the log
- After the log are blocks holding inodes
- After the inode are blocks holding bitmaps tracking  which data blocks are
in use
- The remaining blocks are data blocks; each is either marked free in the bitmap block, or
holds content for a file or directory.


## Buffer Case Layer

1. synchronize access to disk blocks to ensure that only one copy of a block is in memory and that only one kernel thread at a time uses that copy; 
2. cache popular blocks so that they don't need to be re-read from the slow disk. 

The main interface exported by the buffer cache consists of `bread` and `bwrite`
- `bread` obtains a buf containing a copy of a block which can be read or modified in memory
- `bwrite` writes a modified buffer to the appropriate block on the disk

## Logging Layer

系统可能在任何两次磁盘写入之间出现断电或崩溃，因此磁盘上的状态可能只被部分地更新，那么磁盘上的结构就会处于不一致状态。这个问题，称为 **崩溃一致性问题 Crash-Consistency Problem**。

我们用日志的方式来解决这个问题。基本的思路是，每次在更新磁盘，即将覆写相应数据时，先写下一些记录在磁盘的 Log Block，这个记录描述了我们将要做的事情。

假设我们要更新三个块: `target_inode, target_bitmap, target_data`。在将它们写入磁盘的确切位置之前，我们先将其写入日志：
```
┌─────┬──────────────┬───────────────┬─────────────┬───────────┐
│ TxB │ target_inode │ target_bitmap │ target_data │ TxE │ ... │
└─────┴──────────────┴───────────────┴─────────────┴─────┴─────┘
```

可以看到，除了我们需要写入的三个块，还有事务开始标记 `TxB` 和事务结束标记 `TxE`。
- `TxB` 告诉我们有关这次更新的信息，例如三个块最终要写入的地址，并包含事务标识符 `TID`
- 中间三块要写入块的内容，我们确切地将这三块写到了磁盘的日志上
- `TxE` 是这次事务结束的标记，同样包含 TID

一旦以上完整的结构，即 `TxB -> TxE` 安全地存在于磁盘的日志上，这时就可以开始覆写磁盘上文件系统的旧数据了

The log resides at a known fixed location, specified in the superblock.  It consists of a header block followed by a sequence of logged blocks.
The header block contains an array of sector numbers, one for each of the logged blocks, and the count of log blocks. 
Xv6 writes the header block when a transaction commits. Thus a crash midway through
a transaction will result in a count of zero in the log’s header block; a crash after a commit will result in a non-zero count.

此外，为了降低大量额外的磁盘流量，一些文件系统不会一次一个地向磁盘提交每个更新，而是将所有更新缓冲到全局事务中，最后提交包含所有更新的单个全局事务。因此，和延迟写入类似，通过缓冲更新，文件系统在很多情况下避免了对磁盘过多的写入流量。


Xv6 dedicates a fixed amount of space on the disk to hold the log. The total number of blocks written by the system calls in a transaction must fit in that space.
- No single system call can be allowed to write more distinct blocks than there is space in the log

A large file write may write many data blocks and many bitmap blocks as well as an inode block. Xv6’s write system call breaks up large writes into multiple smaller writes that fit in the log


## Block Allocator

The block allocator provides two functions: 
- `balloc` allocates a new disk block
- `bfree` frees a block

As with much of the coc  de described in the remainder of this chapter, balloc and bfree must
be called inside a transaction.

## Inode Layer

  