## Dynamical allocation

Dynamical allocation: physical memory is dynamically allocated to process as they enter and exit the system. (All suffer from external fragmentation 
    - Best fit
    - First fit
    - Next fit
    - Worst fit
What if cannot find a space big enough to run a process? Either because of fragmentation or physical memory is not enough. Can compact and relocate processes (easy with base and bounds hardware, not so easy for direct physical address machines)

## Paging

- (physical) page frame 
- (virtual) page table entry 
    - entry(..., caching disabled, referenced, modified, protection, present, page frame)
    - [caching disabled] is important for pages that map onto device registers rather than memory 
    - virtual addr: entry index <> offset 
- translation lookaside buffer (usually inside MMU
    - each entry contains information about one page 
        - when a virtual addr is presented to the MMU the hardware check TLB by comparing it to all the entry simultaneously; if a valid match is found then there is no need to go to the page table 
- multilevel page table 
    - **calc how many levels should the multi-level paging use**
        - the key is that the top level page entries should be within one page
        e.g. for a 48 bit logical addr, page size is 4KB, page entry size is 8 bytes.
            - Then the offset bits should be of length 12.
            - The count of entries per page is (page size / page entry size) = 512 = 2 ^ 9
        
        
        Then the bits for the top-level page should be no less then 9: 0oAAA_BBB_CCC_DDD_OOOO
    - take x64 as an example (in fact only 48 bits are used, the most significant 12 bits are required to be the same with the 13th most significant bit), the page size if 4KB and each entry taks 8 bytes
        - 0oRRRR_AAA_BBB_CCC_DDD_OOOO
            - (12 dummy bits, 9 bits level-4 index, 9 bits level-3 index, 9 bits level-2 index, 9 bits level-1 index, 12 bits offsets
            - The CR3 register stored the physical addr of the level-4 page table, from AAA we get the physical addr of level-3 page table. Repeat this until reaching the level-1 table, then use the offset to get the translated addr 
- Inverted page table
    - entry(page number, pid, control bits, chained pointer)
        - one entry corresponds to one physical frame
        - one table for all process
    - only fixed size of memory for entries are used
        - e.g. with 64 bits addr and 4MB pages, 2^42 entries are used

### Page Size 

for every process, suppose there are n segments and the page size is p bytes, then on average np/2 bytes are wasted (half of the last page is empty

suppose the average size of a process is s bytes, and the page size is p bytes and every page entry requires e bytes, then the overhead is $\frac{s}{p}\ast\ e+p/2$

### access time (using 1-level paging

```rust
fn calc_access_times(tlb: &TLB, page_table: &PageTable, entry_idx: usize) -> usize {
    let mut times = 0;
    // find the target physical addr
    if tlb.has(entry_idx) {
        // done
    } else {
        times += 1; // check page table
        if page_table.has(entry_idx) {
            // done
        } else {
            // page fault handler...
            // back to prev instruction...
            times += calc_access_times(tlb, page_table, entry_idx);
        }
    }
    times + 1 // access the target phsycal addr
}
```

### Memory reference

1.	Running process generates virtual memory references
2.	The hardware first extracts the virtual addr number (VPN) from the virtual address, 
3.	Check the TLB for a match
    a.	If hit, produces resulting physical address and fetches it from memory
    b.	If not,
        i.	The hardware locates the page table in memory using the page table base register
        ii.	Looks up the page table entry (PTE) for this page using the VPN as an index
            1.	If the page is valid and present in physical memory, the hardware extracts the page frame number (PFN) from the PTE, installs it in the TLB. Retires the user program instruction, this time, there will be a TLB hit
            2.	If the page is swapped to disk, then OS receives a page fault

### Page fault handler

1.	Trap to OS
2.	Save user registers and process state
3.	Determine that exception was page fault
4.	Check that reference was legal and get the disk address from PTE
5.	Find a free page frame
6.	Issue read from disk to free page frame
    a.	Queue up for disk
    b.	Program disk controller to read page 
    c.	Wait for seek and latency
    d.	Transfer page into memory
As soon as program controller, allocate CPU to another process
7.	Take disk transfer completed interrupt
8.	Save user registers and process state
9.	Determine that interrupt was a disk interrupt
10.	Find process and update page tables: mark the page as present and update the PFN field of PTE
    (the TLB may either be updated when swapping back or when retires the user program instruction
11.	Reschedule CPU
12.	Restore process state and resume execution (retry the last instruction
