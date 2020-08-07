![img](D:\learning-notes\Algorithms\Collision-Resolution-Techniques-1.png)



![image-20200704183226472](D:\learning-notes\Algorithms\hash.assets\image-20200704183226472.png)


quadratic probing:
$$H+1^{2}, H+2^{2}, H+3^{2}, H+4^{2}, \ldots, H+k^{2}$$

 it better avoids the clustering problem that can occur with linear probing, although it is not immune. It also provides good memory caching because it preserves some locality of reference; however, linear probing has greater locality and, thus, better cache performance.[

double hashing:
by using a secondary hash of the key as an offset when a collision occurs.
like linear probing be the step is determined by the second hash function

the interval depends on the data, so that values mapping to the same location have different bucket sequences; this minimizes repeated collisions and the effects of clustering.

$$h(i, k)=\left(h_{1}(k)+i \cdot h_{2}(k)\right) \bmod \mid T$$


lazy deletion:


if there is an empty slot, we are sure to find it 

the size of the resulting primary cluster may be very big due to the annexation of the neighboring cluster

linear probing can create large primary clusters that will increase the running time of find/insert/delete operations 

to reduce primary clustering we can modify the probe sequence to `(hash(key) + i * d) % m`
    - where d is some contant integer > 1 and is co-prime to m, 
    - since d and m are co-primes, the probe sequence covers all the slots in the hash table 



cluster:
a cluster is a collection of consecutive occupied slots

a cluster that covers the home address of a key is called the primary cluster

clusters are formed along the path of probing, instead of around the home location

	- these clusters are called secondary clusters
	- secondary clusters are formed as a result of using the same pattern in probing all keys(if two keys have the same home location, their probe seqs are going to be the same

to reduce secondary clustering, use double hashing 

Criteria for choosing a good hash function: 

• it should distribute keys roughly uniformly into slots, 

• regularity in key distribution should not affect the uniformity.



