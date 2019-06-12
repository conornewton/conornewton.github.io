---
title: Lean - √2 is irrational
---

This is my first attempt to deviate from Lean's tutorial and prove something using my own brain. 

I first examine the standard proof of the irrationality of √2 and attempt to formalise this. The proof proceeds as follows.


Suppose for contradiction that √2 is rational. For some \\(a \in \mathbb{Z}\\) and \\(b \in \mathbb{N}\\) we have that $$\sqrt{2} = \frac{a}{b}$$ and $$gcd(a,b)=1$$. We get that $$b^2 = 2a^2$$

