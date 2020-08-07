HDD
    Surfaces
        Tracks
            Sectors

disk_access_t = seek_t (+ controller overhead_t) + rotation_latency_t + transfer_t

\- seek_t: time to target track
    Depends on seek speed & number of tracks the arm moves

\- rotation_latency_t: time to target sector
    depends on rotational speed and how far the sector is from the head

calc speed (depends on RPM, total surfaces, tracks per surface, sectors per track, bytes per sector

e.g. give a disk with 3600 RPM and 4 surfaces, each surface has 64 tracks, each track has 128 sectors, each sector holds 256 bytes. Calculate the speed of this disk (bytes per second
​  rps = 3600 / 60
​  sectors_per_sec = rps * sectors_per_track
​  bytes_per_sec = sectors_per_sec * bytes_per_sector
​  bytes_per_sec = 4 * bytes_per_sec (4 surfaces => 7.5 MB

an important example: https://www.geeksforgeeks.org/gate-gate-cs-2015-set-2-question-59/

 

File allocation
\- Continuous allocation
    \- One entry(start, size) for each file 
    \- Disadvantages:
        \- External fragmentation
        \- It’s nessceray to declare file size when creating

\- Linked allocation
    \- One entry(start, size) for each file
    \- Disadvantages:
        \- Internal fragmentation
        \- Overhead of maintaining next_bloack pointers in each block
        \- No support for random or direct access
          Many seeks are needed to access every block individually

\- Indexed allocation: use an index block to store pointers to each block
    \- Disadvantages:
        \- Pointers overhead is greater 
        \- Inefficient for small files, since one block will be used for index 
    For large files that one index block is not able to hold all the pointers:
        1. Linked scheme: link two or more index blocks together, every block contains a pointer to next index block 
        2. Multilevel index
        3. Combined scheme
            a. A special block `inode` contains all the information

![img](D:\learning-notes\Operating-Systems\fs&disk-scheduling.assets\clip_image002.jpg)

 

Disk (I/O) scheduling algorithm

\- FCFS
\- Shortest seek time first
    \- Overhead of calc
    \- Starvation
    \- High variance of response time
\- SCAN: after reaching the end of disk, reverse direction
    \- Long wait time for locations just visited
    \- The requests at the midrange are serviced more;
\- CSCAN: instead of reversing the direction when reaching one end, move the head to the other end
\- LOOK: based on SCAN, before continue walking, it will check if there are left in current direction
\- CLOOK: LOOK version of CSCAN