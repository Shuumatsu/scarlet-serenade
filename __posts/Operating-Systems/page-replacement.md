Swap Space: space reserved on the disk for moving pages back and forth
    - It should be noted that swap space is not the only on-disk location for swapping traffic (e.g. Executable code. Can just use executable file on disk as backing store. (Problem: recompilation).
When replacements really occur, it is kind of like the HashMap implementation: there are some kind of high-water mark and low-water mark


Local versus global allocation policies
    - Local: every process is associated with a fixed number of pages
        - Thrashing may occur when there are empty pages in system
        (for working set model, only a local strategy makes sense
- Global
    - // todo
    - Arrange pages for every process based on its page fault frequency (PFF)
        - Basic idea: a process should have an ideal page fault frequency. If it faults too often, need to give it more physical page frames. If it faults too infrequently, it has too many physical page frames and need to take some from it and give to other process. When all processes fault too frequently, choose one to swap out.

Page replacement algorithm
- The optimal: 
    - chose by when the page will be referenced next time
- the not recently used:
    - when a clock interrupt occurs, clear the R-bit, then when a page fault occurs, there are 4 possible states, the priority is from a to d:
        a. R = 0, M = 0
        b. R = 0, M = 1
        c. R = 1, M = 0
        d. D = 1, M = 1
- The first-in first-out
    - Belady's anomaly - adding more physical memory may actually make paging algorithm behave worse
        - e.g. reference sequence of 1, 2, 3, 4, 1, 2, 5, 1, 2, 3, 4, 5
        - Physical frame fits 4 pages, 10 page faults total
        - Physical frame fits 3 pages, 9 page faults total
- the least recently used page replacement algorithm
    - how to implement LRU? Two strategies
        - build a clock and mark each page with the time every time it’s accessed
        - move page to front of list every time it is accessed
    both are impractical – overhead is too large
    - the not frequently used
        - every page is associated with a count initialized to zero, when a clock interrupt occurs, add the R-bit to the counter then reset the R-bit
            - the problem is that an once heavily used old age page may still have a very high count
- The second-chance
    - Instead of simply picking the oldest page, use a second chance bit
    - if the second chance bit is 1, 
        - then set it to 0 and put it to the end of the list
        - else discard this page and push a new page to the end of this list
    ```
    pub fn calc_page_faults(references: Vec<usize>, cap: usize) -> usize {
        use std::collections::{HashMap, VecDeque};
        let mut in_memory = VecDeque::new();
        let mut second_chance: HashMap<usize, bool> = HashMap::new();
        let mut page_faults_count = 0;
        for page_number in references {
            if in_memory.contains(&page_number) {
                second_chance.insert(page_number, true);
                continue;
            }
            page_faults_count += 1;
            if in_memory.len() < cap {
                second_chance.insert(page_number, false);
                in_memory.push_back(page_number);
                continue;
            }
            loop {
                let pending = in_memory.pop_front().unwrap();
                if second_chance[&pending] {
                    second_chance.insert(pending, false);
                    in_memory.push_back(pending);
                } else {
                    second_chance.remove(&pending);
                    second_chance.insert(page_number, false);
                    in_memory.push_back(page_number);
                    break;
                }
            }
        }
        page_faults_count
    }
    ```
- the clock 
    - use a ring instead of a list, so no need to put the page to the end of the list, just move the pointer
- aging
    - instead of simply adding to the counter, right shift the counter then add to the most significant bit
- the working set
    - work set is based on current time t and a time interval delta_t
    - every page is associated with a last_t
    - a periodic clock interrupt will clear the R-bit on every entry ? (buqueding
    - if the R-bit is 0 then check if the last_t is within curr_t – delta_t
        - then [pending] else [replace]
          (if no one is replaced, chose the oldest one from pendings 

