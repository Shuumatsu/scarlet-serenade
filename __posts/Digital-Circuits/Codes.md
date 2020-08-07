code is a group of symbols 

## complement 

### r's complement & (r - 1)'s complement 

given base r and number N of n digits. the r's complement of N is $ r^n - N $
    e.g. r = 8, N = 574, n = 3. the r's complement of N is $ 1000_8 - 574_8 = 204_8 $

given base r and number N of n digits. the r's complement of N is $ (r^n - 1) - N $
    e.g. r = 8, N = 574, n = 3. the r's complement of N is $ 777_8 - 574_8 = 203_8 $

#### 1's & 2's Complement

1's complement 其实是 (2 - 1)'s complement。给定一个二进制数，它的 1's complement 其实就是各位取反。
     e.g. 1's complement of $ 0101_2 $ is $ 1111_2 - 0101_2 = 1010_2 $

2's complement 可以由 1's complement + 1 得到，即取反加一

##### Shortcut for 2's Complement

因为最后的加 1 操作只会影响到从 LSB 开始到第一个 1 那些位，所以我们可以有下述算法

1. Write down the given number
2. Starting from LSB, copy all the zeros till the first 1
3. Copy the first 1
4. Complement the remaining bits

这个的原理和使用 a & ((~a) + 1) 来快速判断一个数是否是 2 的指数是相似的

## classification of codes

1. weighted codes - each position has fixed weight
    e.g. binary code, 9421 code, 2421 code

2. non-weighted codes
    e.g. xs-3 code, gray code

3. reflective codes (also called self-containing code)
    e.g. a type of code is called self-containing iff 9's complement is 0, 8's complement is 1 and so on. 
    对于 weighted codes 来说，它是 reflective codes 的充要条件是每一位的权重之和为 9
    eg. 2421 code, xs-3 code

4. sequential codes
    e.g. 8421 code, xs-3 code


5. alphanumeric code 
    e.g. ascii code

6. error detecting & correcting code 
    e.g. Hamming code


## binary coded decimal codes 

each decimal digit (0 - 9) is represented by 4 bit binary number 
    e.g. 8421 code

since 4 bits can represent up to 16 numbers, so t here are valid BCD (0 - 9) and invalid BCD (10 - 15)

### conversion from each decimal number to packaged BCD and vice versa

converse every digits of decimal number and then concat them together 
    e.g. $ 17_{10} $ is converted to $ 0001\_0111 $

the reverse direction shares the same logic with conversion from each decimal number to packaged BCD.

当拿到的二进制表示的长度不是4的倍数时，在前面补0。
    e.g. eg. $ 1\_0111$ is regarded as $ 0001\_0111$ and then converted to $ 17_{10}$

### binary coded decimal codes addition 

there are 3 cases when adding 2 BCD (把他们当成普通的 2 进制数计算


1. sum <= 9, carry = 0, which means the result is in `0..=9`
    this case is correct 

2. sum > 9, carry = 0, which means the result is in `10..=15`
    this case is incorrect, consider $ (9 = 1001)_{8421} + (1 = 0001)_{8421} = 1010 $, and the carry is 0. this is in fact an invalid BCD. We will fix this answer by adding 6

3. carry = 1, which means the result is in `16..=19`
    this case is incorrect, consider $ 9_{8421} + 8_{8421}$, we will fix this answer by adding 6


**But why adding 6 will fix the result?**
    1. 对 sum > 9, carry = 0 也就是 `10..=15` 区间内的结果来说，
        its correct value should be `result - 10` with carry bit set to 1. in hexadecimal, substracting 10 is the same with adding 6 
        加 6 可以正确设置个位，将 carry 位（也就是十位）设为 本次运算的 cout
    2. 对 carry = 1, 也就是 `16..=19`，其实我们拿到的个位是在区间 `0..=3` 区间内的，加 6 可以正确设置个位，carry 位继续沿用第一次运算的 cout 即可

### the double dabble algorithm (also called the shift-and-add-3 algorithm 

the double dabble algorithm is used to convert binary numbers in to BCD 

```
100s Tens Ones   Original
0010 0100 0011   11110011
```

the algorithm operates as follows:

1. suppose the original number to be converted is n bits wide. reserve a scratch space wide enough to hold both the original number and its BCD representation

the scratch space is initialized to all zeros, and then the value to be converted is copied into the original space on the right

2. the algorithm then iterates n times. on each iteration, any BCD digit which is greater than 4 is incremented by 3; then the entire scratch space is left-shifted one bit

take 243 as an example, the whole steps are as follows:

```
100s Tens Ones   Original
0000 0000 0000   11110011   Initialization
0000 0000 0001   11100110   Shift
0000 0000 0011   11001100   Shift
0000 0000 0111   10011000   Shift
0000 0000 1010   10011000   Add 3 to ONES, since it was 7
0000 0001 0101   00110000   Shift
0000 0001 1000   00110000   Add 3 to ONES, since it was 5
0000 0011 0000   01100000   Shift
0000 0110 0000   11000000   Shift
0000 1001 0000   11000000   Add 3 to TENS, since it was 6
0001 0010 0001   10000000   Shift
0010 0100 0011   00000000   Shift
   2    4    3
```

**why it works???**


### 2421 code 

2521 code 的同一个数可以有不同的表示，例如 5 可以表示为 0101 或者 1011. 我们在选择的时候倾向于使得 2421 码是 self-containing 的那种。

|Decimal|0|1|2|3|4|5|6|7|8|9|
|--- |--- |--- |--- |--- |--- |--- |--- |--- |--- |--- |
|2421 code|0000|0001|0010|0011|0100|1011|1100|1101|1110|1111|

### XS-3 code (Excess-3 code

加三码由 8421 码加三得到，例如 3 在 8421 码中的表示位 0011，在加三码中的表示为 0110

<table><tbody><tr><th>Decimal</th><th>Binary</th><th>Decimal</th><th>Binary</th><th>Decimal</th><th>Binary</th><th>Decimal</th><th>Binary</th></tr><tr align="center"><th>−3</th><td>0000</td><th>1</th><td>0100</td><th>5</th><td>1000</td><th>9</th><td>1100</td></tr><tr align="center"><th>−2</th><td>0001</td><th>2</th><td>0101</td><th>6</th><td>1001</td><th>10</th><td>1101</td></tr><tr align="center"><th>−1</th><td>0010</td><th>3</th><td>0110</td><th>7</th><td>1010</td><th>11</th><td>1110</td></tr><tr align="center"><th>0</th><td>0011</td><th>4</th><td>0111</td><th>8</th><td>1011</td><th>12</th><td>1111</td></tr></tbody></table>

#### addition 

首先考虑两数相加的结果没有进位的情况：
    两个加三码相加和原始的 8421 码比起来最后的结果相比增加了 6， 所以要得到结果的加三码表示需要将结果减 3

如果两数相加的结果产生了进位，则我们需要将最后的结果加三
    从表格中可以看出来，1111 代表13。产生进位的时候相当于我们的结果被扣除了 13， 但是对于一个 BCD 来说进位的时候实际应该扣除 10，所以最后的结果需要加 3 
    e.g. 4 + 9 也就是 0111 + 1100 得到 0011 也就是 0 并产生进位。但最后的结果应该是 3, 所以我们需要加 3

### Gray Code 

also known as Reflected Binary Code & Unit Distance Code & Minimum Error Code & Cyclic Code...

Two successive values differ in only 1 bit. Binary code is converted to Gray Code to reduce swtiching,
    e.g. Converting from $7_{10} = 0111_2$ to $8_{10} = 1000_2$ needs 4 switchings in Binary Code, while in Gray Code converting from $7_{10} = 0100_{gray}$ to $8_{10} = 1100_{gray}$ needs only 1 

#### Binary to Gray Code Conversion 

suppose `x` is a 4 bit number: `x = (b1, b2, b3, b4)`, `y = (g1, g2, g3, g4)` is the converted. 
The steps are as follows:
    1. Record the MSB as it is.
        `g1 = b1`
    2. Repeat: add the MSB of the binary to the next bit, record the sum and neglect the carry (this is infact xor
        ```
            for i in 2..=4 {
                g[i] = b[i - 1] ^ b[i]
            }
        ```

Notice that in both conversions we are always adding current MSB from the **binary** to the next bit from the to be converted


### Parity Check 

奇偶校验，除了数据外添加了额外的一个校验位。可以检测一位的错误。

There are two types of Parity
    - 偶校验，如果数据部分中只有奇数个 1 的话，将校验位设为 1
    eg. 0100 will be decoded into **1**0100 (in this case, the first bit is used as the parity bit)
    - 奇校验，如果数据部分中只有偶数个 1 的话，将校验位设为 1


### Hamming Code

Hamming codes can detect up to two-bit errors or correct one-bit errors without detection of uncorrected errors.

<table><tbody><tr><th colspan="2"><span>Bit position</span></th><th><span>1</span></th><th><span>2</span></th><th><span>3</span></th><th><span>4</span></th><th><span>5</span></th><th><span>6</span></th><th><span>7</span></th><th><span>8</span></th><th><span>9</span></th><th><span>10</span></th><th><span>11</span></th><th><span>12</span></th><th><span>13</span></th><th><span>14</span></th><th><span>15</span></th><th><span>16</span></th><th><span>17</span></th><th><span>18</span></th><th><span>19</span></th><th><span>20</span></th><td rowspan="7">…</td></tr><tr><th colspan="2">Encoded data bits</th><th>p1</th><th>p2</th><th>d1</th><th>p4</th><th>d2</th><th>d3</th><th>d4</th><th>p8</th><th>d5</th><th>d6</th><th>d7</th><th>d8</th><th>d9</th><th>d10</th><th>d11</th><th>p16</th><th>d12</th><th>d13</th><th>d14</th><th>d15</th></tr><tr><th rowspan="5">Parity<br />bit<br />coverage</th><th>p1</th><td data-sort-value="No">X</td><td></td><td data-sort-value="No">X</td><td></td><td data-sort-value="No">X</td><td></td><td data-sort-value="No">X</td><td></td><td data-sort-value="No">X</td><td></td><td data-sort-value="No">X</td><td></td><td data-sort-value="No">X</td><td></td><td data-sort-value="No">X</td><td></td><td data-sort-value="No">X</td><td></td><td data-sort-value="No">X</td><td></td></tr><tr><th>p2</th><td></td><td data-sort-value="No">X</td><td data-sort-value="No">X</td><td></td><td></td><td data-sort-value="No">X</td><td data-sort-value="No">X</td><td></td><td></td><td data-sort-value="No">X</td><td data-sort-value="No">X</td><td></td><td></td><td data-sort-value="No">X</td><td data-sort-value="No">X</td><td></td><td></td><td data-sort-value="No">X</td><td data-sort-value="No">X</td><td></td></tr><tr><th>p4</th><td></td><td></td><td></td><td data-sort-value="No">X</td><td data-sort-value="No">X</td><td data-sort-value="No">X</td><td data-sort-value="No">X</td><td></td><td></td><td></td><td></td><td data-sort-value="No">X</td><td data-sort-value="No">X</td><td data-sort-value="No">X</td><td data-sort-value="No">X</td><td></td><td></td><td></td><td></td><td data-sort-value="No">X</td></tr><tr><th>p8</th><td></td><td></td><td></td><td></td><td></td><td></td><td></td><td data-sort-value="No">X</td><td data-sort-value="No">X</td><td data-sort-value="No">X</td><td data-sort-value="No">X</td><td data-sort-value="No">X</td><td data-sort-value="No">X</td><td data-sort-value="No">X</td><td data-sort-value="No">X</td><td></td><td></td><td></td><td></td><td></td></tr><tr><th>p16</th><td></td><td></td><td></td><td></td><td></td><td></td><td></td><td></td><td></td><td></td><td></td><td></td><td></td><td></td><td></td><td data-sort-value="No">X</td><td data-sort-value="No">X</td><td data-sort-value="No">X</td><td data-sort-value="No">X</td><td data-sort-value="No">X</td></tr></tbody></table>

Hamming Code 的原理是每一个整数（位置）都可以由一个 n 位长的二进制数表示。我们将每个 2 的次幂的位置设为校验位，那么每个数据根据其位置可以由多个校验位验证。
    e.g. 例如第七位，其二进制表示为 0111，那么第一个第二个以及第三个校验位，也就是 p1, p2, p4 会负责校验它

由表格可以看出，如果出错的一位是校验位的话，那么只有这个校验位本身会检测到错误，我们可以直接定位到出错的位置并改正。
如果出错的一位不是校验位的话，多个校验位会检测到错误，我们只需要将校验位按顺序连接起来得到二进制数就可以定位到出错的位置并改正。









