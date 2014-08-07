
# coding: utf-8

# # Chapter 2 Exercises
# 
# ## 3. Bias/variance/irreducible error curves
# 
# ### a) Sketch
# 
# Fuck it I'm doin it live.
# 
# ### b) Explain
# 
# The curve for bias is high for low flexibility, and low for high flexibility, because a more flexible model can approximate more true underlying functions, while a simple model will be systematically wrong for many functions that are actually complex (or complex in the wrong ways).  The variance curve is the opposite, because for a highly flexible model the predictions vary a lot depending on the training data, while a simple model can be estimated with more stability using less data.  The irreducible/Bayes error is constant for a given underlying function.  
# 
# The training error decreases with increasing complexity because a more complex model can fit arbitrary data (including the error variance).  The test error, however, has a "U" shape, being high for models that are too simple (can't capture the form of the underlying function, i.e. bias) and models that are too complex (capture noise as signal).  The lowest point of the test error is the irreducible error (assuming that the true function is somewhere in the family of functions spanned by the x-axis).
# 
# ## 6. Parametric vs. non-parametric
# 
# Parametric approaches assume that the underlying function belongs to a particular family of functions, such that it can be described by the values of a couple of _parameters_.  Non-parametric approaches do not make this assumption, and generally adjust based on the underlying complexity of the data.  Parametric models have some fixed level of complexity that has to be determined ahead of time (or based on, e.g., model comparison), while non-parametric models can often achieve arbitrary levels of complexity if its warranted by the data.
# 
# Because of this greater flexibility, non-parametric models are very useful when the underlying function is, in fact, very complex.  However, there needs to be enough data available to actually _estimate_ the model.  Parametric models, because they are constrained in their complexity, can (in general) be estimated using less data and are less prone to over-fitting (although it's possible to have very complex parametric models).  By having a fixed set of degrees of freedom, a parametric model provides a priori constraints which are helpful when there's not a lot of data available.
# 
# ## 7. K-nearest neighbors
# 
# ### a) Compute Euclidean distance

# In[1]:

@show X = hcat([0, 2, 0, 0, -1, 1], [3, 0, 1, 1, 0, 1], [0, 0, 3, 2, 1, 1])
@show Y = [:red, :red, :red, :green, :green, :red]

# unlike R, Julia does not automatically drop extra dimensions, so we need
# to make sure that x0 is the same dimensions of the slices of X that we
# will be subtracting to find the distance.
x0 = zeros(1, 3)

dist = [sqrt(sum((X[ii,:] - x0).^2)) for ii in 1:size(X,1)]
# alternatively you can also use `squeeze(X[ii,:], 1)` to drop all but one 
# dimensino, and declare x0 as an array literal like [0, 0, 0]


# ### b) K=1

# In[2]:

@show dist_order = sortperm(dist)

Y[dist_order[1]]


# The answer would be "Green", since this is the `Y` value of the closest training point.
# 
# ### c) K=3

# In[3]:

Y[dist_order[1:3]]


# For $K=3$, the answer would be "Red", because the next two closest points are "Red".
# 
# ### d) Best $K$ for non-linear boundary
# 
# Lower $K$ is better for highly non-linear Bayes decision boundary.  Very high $K$ produces nearly linear boundaries, while very low $K$ basically follows each individual training exemplar. 

# ## 8. Analysis of the College data set
# 
# ### a) Load data
# 
# Here I'm using the `dataset` function provided by `RDatasets` package (which just reads the data from the R dataset package that the authors of the book put together here.

# In[1]:

using Gadfly, RDatasets
college = dataset("ISLR", "College")


# ## b) Move first column to row names
# 
# In the dataset in the `ISLR` package, this is already done.
# 
# ## c) Summary and plots
# 
# First, just a quick summary of the data frame.

# In[2]:

describe(college)


# There's not equivalent of the `pairs()` function in R, as far as I can tell.  But perhaps it's possible to hack something together that will do the same thing...

# In[3]:

function pairs(data::DataFrame, cols=[1:ncols(d)])
    names = colnames(data)
    nr = nrow(data)

    # construct new DataFrame
    d = DataFrame()
    for i in cols
        for j in 1:i-1
            d = vcat(d, 
                     DataFrame(xVar=rep(names[i], nr), yVar=rep(names[j], nr), x=data[names[i]], y=data[names[j]]))
        end
    end

    # okay this works but it blows up and crashes the kernel...
    set_default_plot_size(10inch, 10inch)
    plot(d, x=:x, y=:y, xgroup=:xVar, ygroup=:yVar, 
        Geom.subplot_grid(Geom.point, free_y_axis=true, free_x_axis=true))
end

# this works in the strict sense of producing the plot, but it causes _problems_
# because of the default back end that I can't figure out how to change :(
#pairs(college, [2:4])


# Boxplot of out-of-state tuition for public vs. private universities.

# In[4]:

set_default_plot_size(4inch, 4inch)
plot(college, x=:Private, y=:Outstate, Geom.boxplot)


# Add the `Elite` variable (more than 50% of entering students in top 10% of graduating class).
# 
# There's a couple ways to do this.  You frustratingly can't create a new `DataFrame` with the R syntax of calling the constructor with an existing data frame and keyword arguments for new variables.  But the concatenation functions work as you'd expect (can horizontally concatenate two `DataFrame`s together), you can assign to non-existent columns, and there's a nice in-place version of the `within` function which will add new variables based on expressions evaluated within the context of a `DataFrame`.
# 
# Actuall, scratch that: `within` and friends were removed from the DataFrames package after the v0.2 release (discussion [here](https://github.com/JuliaStats/DataFrames.jl/pull/492)).

# In[5]:

# can add variables just like in R, by assigning to non-existent columns
college[:Elite] = college[:Top10perc] .> 50.0

# a slightly more elegant way of constructing this variable: 
# (you need to use the elementwise comparison operator .> because
# it broadcasts).
#
# HAHA JK this function was removed in a recent update :(
#within!(college, :(Elite = Top10perc .> 50))


# In[6]:

mean(college[:Elite])


# In[7]:

plot(college, x=:Elite, y=:Outstate, Geom.boxplot)


# In[8]:

set_default_plot_size(10inch, 3inch)
hstack(plot(college, x=:Outstate, Geom.histogram), 
       plot(college, x=:Accept, Geom.histogram), 
       plot(college, x=:PhD, Geom.histogram))


# ## 9. Analysis of `Auto`
# 
# Start by loading and cleaning (removing cases where there's `NA`)

# In[9]:

auto = dataset("ISLR", "Auto")


# The version in the dataset package only includes the complete cases.  Let's read the .csv file and see how that works.  Because the NAs are indicated using `"?"`, we need to specify this in the `nastrings` argument:

# In[10]:

auto2 = readtable("data/Auto.csv", nastrings=["", "NA", "?"])
dump(auto2)
auto2[~ complete_cases(auto2), :]


# We can use the in-place version `complete_cases!` to remove any rows with missing data: 

# In[11]:

complete_cases!(auto2)
size(auto2)


# ### a. Quantitate and qualitative predictors

# In[12]:

dump(auto2)


# Looks like a few of the numeric variables are basically qualitative, like cylinders and origin (not sure which that is supposed to represent but there's only three values it takes).  In Julia, qualitative data can be represented using `PooledDataArray`s, which are like factors in R.  The `pool` function transforms from normal to pooled data structures (and the in-place version used below is convenient for a data frame).

# In[13]:

# to pool manually: 
#auto2[:cylinders] = PooledDataArray(auto2[:cylinders])
#auto2[:origin] = PooledDataArray(auto2[:origin])
# pool in place: 
pool!(auto2, [:cylinders, :origin, :name])
dump(auto2)
@show levels(auto2[:cylinders])
@show levels(auto2[:origin])


# ## b. Range of quantitative predictors
# 
# To get the range of all the quantitative variables, first define a function that tells whether a column is quantitative or not (numeric and not `PooledDataArray`).  Then use it to filter the columns, and apply the `extrema` function (like `range()` in R).

# In[42]:

function is_quant(x)
    get_ipython().system(u'(typeof(x) <: PooledDataArray) & (eltype(x) <: Number)')
end

auto2_quant = [is_quant(col) for (name, col) in eachcol(auto2)]

[(name, extrema(col)) for (name, col) in eachcol(auto2[auto2_quant])]


# ## c. Mean and standard dev. of quantitative predictors
# 
# Using a list comprehension (not so pretty).

# In[43]:

{name => (mean(col), std(col)) for (name, col) in eachcol(auto2[auto2_quant])}


# Using `stack` (like `melt` in R), and `by` (like `ddply`).  Note that `stack` and `melt` both exist and do similar things, but `stack` requires only measure variables and `melt` requires only id variables.

# In[45]:

function mean_std_range_quant_vars(df) 
    quant_vars = [is_quant(col) for (key, col) in eachcol(df)]
    stacked_quants = stack(df[quant_vars], names(df[quant_vars]))
    by(stacked_quants, :variable, d -> DataFrame(mean=mean(d[:value]), std=std(d[:value]), range=extrema(d[:value])))
end

mean_std_range_quant_vars(auto2)


# ## d. Removing rows 10-85

# In[47]:

mean_std_range_quant_vars(auto2[ !(10 .<= 1:nrow(auto2) .<= 85), :])


# ## e. Graphical summary

# In[18]:

set_default_plot_size(4inch, 4inch)
plot(auto2, x=:weight, y=:mpg, Geom.point)


# In[19]:

plot(auto2, x=:cylinders, y=:weight, Geom.point)


# In[20]:

plot(auto2, x=:horsepower, Geom.histogram)


# In[21]:

plot(auto2, x=:horsepower, y=:displacement)


# ## f. Predicting MPG from other variables
# 
# Lots of variables are related to MPG: mostly decreasing MPG as they increase.  But later model years have higher MPGs, pretty uniformly.

# In[22]:

set_default_plot_size(10inch, 4inch)
plot(melt(auto2, :mpg, [:horsepower, :displacement, :weight, :cylinders, :year]), 
x=:value, y=:mpg, xgroup=:variable, Geom.subplot_grid(Geom.point, free_x_axis=true))


# # 10. Boston data
# 
# ## a) Load the data

# In[23]:

boston = dataset("MASS", "Boston")


# Size of the dataset (rows, columns)

# In[24]:

size(boston)


# Each row is one municipality (?) that's a suburb of Boston, and this is the description of the columns copied from the description of the dataset because I'm lazy:
# 
#  *    ‘crim’ per capita crime rate by town.
#  *    ‘zn’ proportion of residential land zoned for lots over 25,000 sq.ft.
#  *    ‘indus’ proportion of non-retail business acres per town.
#  *    ‘chas’ Charles River dummy variable (= 1 if tract bounds river; 0 otherwise).
#  *    ‘nox’ nitrogen oxides concentration (parts per 10 million).
#  *    ‘rm’ average number of rooms per dwelling.
#  *    ‘age’ proportion of owner-occupied units built prior to 1940.
#  *    ‘dis’ weighted mean of distances to five Boston employment centres.
#  *    ‘rad’ index of accessibility to radial highways.
#  *    ‘tax’ full-value property-tax rate per \$10,000.
#  *    ‘ptratio’ pupil-teacher ratio by town.
#  *    ‘black’ 1000(Bk - 0.63)^2 where Bk is the proportion of blacks by  town.
#  *    ‘lstat’ lower status of the population (percent).
#  *    ‘medv’ median value of owner-occupied homes in \$1000s.
#  
#  I think the depdendent variable is meant to be the median home value (`medv`).

# ## b) Make some pairwise scatter plots
# 
# Since we're going to look at crime rates later, let's look at some comparisons with that.

# In[25]:

# add an index column
boston[:idx] = [1:size(boston,1)]
# melt in order to compare all other variables to crime:
bos_crime = melt(boston, [:Crim, :idx])


# In[26]:

set_default_plot_size(30cm, 6.5cm)
plot(bos_crime, x=:value, y=:Crim, xgroup=:variable, Geom.subplot_grid(Geom.point, free_x_axis=true))


# Hard to see with the scatterplots because of overplotting, so let's look at a 2d histogram

# In[27]:

plot(bos_crime, x=:value, y=:Crim, xgroup=:variable, Scale.continuous_color,
Geom.subplot_grid(Geom.histogram2d(xbincount=10, ybincount=10), free_x_axis=true))


# Another issue is that there's a lot of really low crime rates, and a few that are appreciably higher.  Maybe a log-transformed measure would be better.

# In[28]:

# first check whether there are any crime rates that are numerically zero
@show any(boston[:Crim] .== 0)

# add a log-transformed per-capita crime rate
boston[:LogCrim] = log(boston[:Crim])
bos_crime = melt(boston, [:Crim, :LogCrim, :idx])


# In[29]:

plot(bos_crime, x=:value, y=:LogCrim, xgroup=:variable, Geom.subplot_grid(Geom.point, free_x_axis=true))


# In[30]:

plot(bos_crime, x=:value, y=:LogCrim, xgroup=:variable, Scale.continuous_color,
Geom.subplot_grid(Geom.histogram2d(xbincount=10, ybincount=10), free_x_axis=true))


# ## c) Predictors associated with crime rate
# 
# Looking at these plots, the predictors that seem most related to crime rate are: industrial, NOx, age (proportion of buildings before 1940), median home value, "lower status of the population" (?), distance from radial highways, and tax rate.  A couple of these are kind of odd, where there's a cluster of very high crime rates all with the same value of the predictor (e.g. high `Rad`, `Tax`, `PTRatio`, or moderate-high `Indus`).  This seems to be the primary driver of associations with `Rad` (accessibility of radial highways) and `Tax` (property tax per $10,000).
# 
# Ignoring those, though, the other relationships make a lot of sense.  Higher crime rates are associated with older building stock, more pollution (`NOx`), more industrial (or non-retail commercial) land use, lower home values, closer distance to center city.  

# ## d) Range of predictors and outliers

# In[31]:

describe(boston)


# In[32]:

bos_iqr_summary = by(stack(boston, names(boston)), :variable, 
df -> DataFrame(mean=mean(df[:value]), std=std(df[:value]), 
                low=quantile(df[:value], 0.25), high=quantile(df[:value], 0.75)))
bos_iqr_summary[:iqr] = bos_iqr_summary[:high] - bos_iqr_summary[:low]
bos_iqr_summary[:iqrStdRatio] = bos_iqr_summary[:iqr] ./ bos_iqr_summary[:std]

bos_iqr_summary


# The one that really jumps out is crime rates: most places have very low (median is 0.25) but there are a whole bunch in the double digits.  The mean and the 3rd quartile are basically the same, which indicates the presence of some pretty severe high outliers.
# 
# The other one that I notice is `Age`, the proportion of buildings built before 1940.  For most places it's very high, but for a couple it's in the single digits.
# 
# ## e) Border Charles River
# 
# Only about 7% (35 out of 506).

# In[33]:

@show mean(boston[:Chas])
by(boston, :Chas, d -> size(d, 1))


# ## f) Median pupil-teacher ratio

# In[34]:

median(boston[:PTRatio])


# ## g) Lowest median home value

# In[35]:

(low_val, low_val_idx) = findmin(boston[:MedV])
boston[low_val_idx, :]


# In[36]:

# for each variable, get the percentile for the lowest-value row
{key => mean(boston[low_val_idx, key] .> val) for (key, val) in eachcol(boston)}


# Lowest median home value also has high proportion of black residents, pollution, industrial land use, pupil-teacher ratio, crime, "lower status of population", and age of bulidings, and low lot sizes and rooms per dwelling.  Somewhat surprising is that it's also associated with higher property tax rates and lower distance to employment centers, and higher accesibility to radial highways, but the latter two at least might reflect "inner city" status.
# 
# ## h) Big ol' houses

# In[37]:

seven_rooms = boston[boston[:Rm] .>= 7, :]


# In[38]:

size(seven_rooms)


# In[39]:

eight_rooms = boston[boston[:Rm] .>= 8, :]


# In[40]:



