---
layout: page
title: Computational Statistics Vs. Machine Learning
---

I prefer to think that I study "computational statistics" and not "Machine Learning."  In my opinion,
Machine Learning largely refers to some of the more algorithmic aspects of prediction, and not really on the
statistics (e.g. inference).  Recently, I think Machine Learning has come to focus on topics like Neural Networks,
which are not really statistical models, but algorithmic models. 

I think that Machine Learning involves first finding data and choosing an algorithm to fit the model to the data.
Sometimes, these algorithms may include statistical models, such as regression and classification, or 
statistical concepts, such as PCA and out-of-sample prediction; however, the fact remains that the final goal is
to find the best prediction method and not necessarily care about the underlying assumptions.  If a convolutional 
neural network with 1000 layers predicts well, then why worry about the underlying distribution or statistical properties
of the estimator? 

Computational Statistics, on the other hand, is about first choosing a model (defined as a family of distributions),
and performing inference or prediction on the data.  To me, the computational part comes from how that inference or
prediction is formed when the model is difficult to estimate.  For example, linear regression is a statistical model
(where the errors are normally distributed), and under the Gauss-Markov conditions, the MLE is the typical least-squares
estimate that we use.  From a computational standpoint, this is mostly solved, since we have a nice closed-form expression
for the MLE (although we may not actually use this since it involves calculating the matrix inverse).  In more complicated
models, such as the [Stochastic Block Model](https://en.wikipedia.org/wiki/Stochastic_block_model), the focus becomes not
only on the inference, but also on proving things about algorithms, such as consistency.  In some situations,
a convex optimization method is a nicer computational solution than finding the exact MLE, and the focus becomes on
determining how much we "lose" by using a non-exact optimization algorithm.  Can we still prove consistency?  Is
the rate of convergence the same?  

I think that Machine Learning as a field has begun to care about the algorithms and not the underlying assumptions, which
can be a scary place to be if it is wrong.  For harmless situations, such as movie recommendations, this is fine, since
there is no harm done.  But what happens if we are using assumption-less neural networks to recommend drugs to patients
and we find out that the neural network isn't doing what we like?  




