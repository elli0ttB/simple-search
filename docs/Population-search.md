# Descisions
There were a few choices we made in our design.

Both crossover methods generated only 1 child. Our mutation algorithm was the
same as Nic's. We used the same algorithm as Nic, where in each run we march down the bit
string and flip the bit with probability 1/n.

We had population size 100 for all of our trials. The essentials of
metaheuristics seemed to suggest small population sizes.

Our algorithm to generate a next generation combined tournament selection and
the (μ+λ) algorithm with λ=3. We used tournament selection
to generate a group of 100 children, and then repeated this three times, sorted
by score, and picked the best 100 individuals. The
algorithms were implemented composably, so we could easily change how the algorithms
fit together or the paremeters.

We used a tournament size of 2 as Essentials of Metaheurstics suggested.
That is, to create each batch of children, we selected parents by randomly
picking 2 individuals, and generated 1 child from those two
parents, and repeated this `(count population)` times.

Originally, our tournament selection algorithm would only take unique
individuals, by using Clojure's `distinct` function. However,
this lead to the algorithm stalling for high rates of max-evals, so we removed
this feature/bug. However, this means that our populations probably converged
to the same individuals over time, as combining the children with the parents
each genearation would allow the most fit answers to propagate quickly.

To select the best individual, selected the maximum of all the individuals over
all the generations.
