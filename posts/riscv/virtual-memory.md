---
title: Computer Architecture | RISC V | Virtual Memory
---

All harts in a system must employ the same PTE-update scheme as each other

--

supervisor mode 提供了虚拟内存，启用分页的时候大多数地址（包括 load/store 和 pc 中的地址）都是虚拟地址。要访问物理内存，它们必须通过页表被转换为真正的物理地址。

一个页表总是一页大小并且总是按页对齐的。根页表的 physical page number 被存储 在 satp 寄存器中。

---

RISC V 的分页方案以 SvX 命名，其中 X 为虚拟地址的长度。
- 例如 RV32 采用 Sv32 方案，采用了两级页表，每页 4KB
- 例如 RV64 采用 Sv39 方案（通常），采用了三级页表，每页 4KB

以 Sv39 为例，

```
VirtualAddr:
+----------+---------+---------+---------+-------------+
| Not Used | VPN[2]  | VPN[1]  | VPN[0]  | page offset |
+----------+---------+---------+---------+-------------+
| 63 - 39  | 38 - 30 | 29 - 21 | 20 - 12 | 11 - 0      |
+----------+---------+---------+---------+-------------+

PhysicalAddr:
+----------+---------+---------+---------+-------------+
| Not Used | PPN[2]  | PPN[1]  | PPN[0]  | Page Offset |
+----------+---------+---------+---------+-------------+
| 63 - 56  | 55 - 30 | 29 - 21 | 20 - 12 | 11 - 0      |
+----------+---------+---------+---------+-------------+
```

```
Entry: 
+----------+---------+---------+---------+-------+---+---+---+---+---+---+---+---+
| Not Used | PPN[2]  | PPN[1]  | PPN[0]  | RSW   | D | A | G | U | X | W | R | V |
+----------+---------+---------+---------+-------+---+---+---+---+---+---+---+---+
| 63 - 54  | 53 - 28 | 27 - 19 | 18 - 10 | 9 - 8 | 7 | 6 | 5 | 4 | 3 | 2 | 1 | 0 |
+----------+---------+---------+---------+-------+---+---+---+---+---+---+---+---+
```

The U bit indicates whether the page is accessible to user mode。通常情况下 supervisor mode operates with the SUM bit clear，supervisor mode 不能访问用户页，但是如果 SUM bit is set, supervisor mode 也能访问用户页。

The G bit 指定全局映射，意味着该映射存在于所有地址空间中（如果是一个非页 PTE 被设置为 global 则其 subsequent levels of the page table are global.

Each leaf PTE contains an accessed (A) and dirty (D) bit. 
- The A bit indicates the virtual page has been read, written, or fetched from since the last time the A bit was cleared.
- The D bit indicates the virtual page has been written since the last time the D bit was cleared.

---

Virtual Address Translation Process:
```
pub fn virt_to_phys(root: *const Table, vaddr: VirtualAddr) -> Option<PhysicalAddr> {
    let mut table = root;
    for lvl in (1..=2).rev() {
        let entry = unsafe { &(*table).entries[vaddr.extract_vpn(lvl)] };
        if !entry.is_valid() {
            return None;
        }
        let ppn = entry.extract_ppn_all();
        table = PhysicalAddr::from(ppn, 0).as_mut_ptr::<Table>();
    }

    let entry = unsafe { &(*table).entries[vaddr.extract_vpn(0)] };
    let ppn = entry.extract_ppn_all();
    Some(PhysicalAddr::from(ppn, vaddr.extract_offset()))
}
```


