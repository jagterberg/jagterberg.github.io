---
layout: page
title: Joshua Agterberg
description: Graduate Student at Johns Hopkins
---
<div class="container">
	<div class = "span3">
		<div style="text-align:center"><img src ="assets/pics/Inked_joshua_agterberg_8-20.jpg"/>
		</div>
	</div>
	<div class = "span4">
		<div style="text-align:right">
		<a href="https://engineering.jhu.edu/ams/">Department of Applied Mathematics and Statistics</a><br/>
		<a href="https://engineering.jhu.edu/">Whiting School of Engineering</a><br/>
          	<a href="https://www.jhu.edu/">Johns Hopkins University</a><br/>
		<br/>
		<a href="{{ BASE_PATH }}/assets/JoshuaAgterbergCV.pdf">CV</a><br/>
		<a href = "https://github.com/jagterberg">github</a><br/>
		<a href = "https://www.linkedin.com/in/joshuaagterberg/">LinkedIn</a><br/>
		</div>		
	</div>
</div>

<br/>
<br/>

Hello! I am a graduate student at Johns Hopkins working on my master's in applied mathematics and statistics.
I am advised by Professor [Carey Priebe](https://www.ams.jhu.edu/~priebe/).  I am broadly interested in algorithms 
and statistical models for networks, and I am planning to work in this area during my PhD in Statistics.  You can 
read about my projects [here](/pages/projects.html).

This summer, I was a research assistant for Carey Priebe, and I attended the [D3M](https://www.darpa.mil/program/data-driven-discovery-of-models)
workshop in Arlington, VA developing Python code for graph-based problems.  In addition, I have helped develop the [iGraphMatch](https://github.com/dpmcsuss/iGraphMatch)
R package, and I am currently working on investigating the statistical properties of [Vertex Nomination](https://arxiv.org/abs/1711.05610).

I graduated from the University of Wisconsin-Madison in 2017 with my Bachelor of Business Administration in actuarial
science and mathematics.  While there, I was fortunate to be advised by [Margie Rosenberg](https://bus.wisc.edu/faculty/marjorie-rosenberg).
During my senior year, I started some research examining the possibilities of unsupervised learning for insurance data.
As of now, we are finishing up a [project](https://www.soa.org/pd/events/2017/predictive-analytics-symposium/pd-2017-09-predictive-analytics-session-010.pdf) 
involving the use of k-medoids clustering on survey data when variables are all categorical.  

This work has led me to develop my [catDist](https://github.com/jagterberg/catDist) R package for dissimilarity
indices when data are all or almost all categorical. Originally, I intended for the package to provide more context-specific 
dissimilarity metrics than [Gower's distance](https://www.r-bloggers.com/clustering-mixed-data-types-in-r/), but I 
recently discovered the [nomclust](https://cran.r-project.org/web/packages/nomclust/index.html) R package which provides
similar functionality to <code>catDist</code>.  However, the main difference is that many of the algorithms in <code>catDist</code> are written
in C (and not R), which greatly speeds things up. The code is still in development, so practitioners interested in
K-medoids or spectral clustering can use <code>nomclust</code>.

Finally, in my spare time, I like to read good fantasy and play jazz piano.  At UW-Madison, I regularly played piano with 
[The Left Field Quartet](https://leftfieldquartet.bandcamp.com/releases).  



