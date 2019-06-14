---
title: Lean - √2 is irrational
---

This is my first attempt to deviate from Lean's tutorial and prove something using my own brain. 

I first examine the standard proof of the irrationality of $\sqrt{2}$ and attempt to formalise this. The proof proceeds as follows.


Suppose for contradiction that $\sqrt{2}$ is rational. For some $a \in \mathbb{Z}$ and $b \in \mathbb{N}$ we have that $$\sqrt{2} = \frac{a}{b}, gcd(a,b)=1$$

 We get that $$b^2 = 2a^2$$

