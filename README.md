## Complete-Regression-Analysis
Regression analysis with a data violates all major assumptions of OLS

    This is a demo for how to deal with assumption violations in regression analysis. 
First thing to check about your regression model is to check if it's violating assumptions or not.
I choose this data set because it violates all major assumptions.

    Problems:
        1) Violating normality assumption.
        2) Violating homoscedasticity.
        3) Multicollinearity.
        4) 46 observations which is extremely low.
        5) Low R-Squared(around 0.3).

    First I am going to do little bit of data cleaning.
    
    I'll use linear, lasso, ridge, elasticnet and weighted regression as well as Box-Cox transformation, principal component analysis(PCA). 
    
    Finally I will use Leave-One-Out Cross Validation to measure the performance of the model because we are low on observations.

#
#  Reference:
#
#    Helmut Spaeth,
#    Mathematical Algorithms for Linear Regression,
#    Academic Press, 1991,
#    ISBN 0-12-656460-4.
#
#    K Brownlee,
#    Statistical Theory and Methodology,
#    Wiley, 1965, pages 464-465.
#
#  Discussion:
#
#    In various states, population and drinking data was recorded.
#
#    There are 46 rows of data.  The data includes:
#
#      I,  the index;
#      A0, 1;
#      A1, the size of the urban population,
#      A2, the number of births to women between 45 to 49
#          (actually, the reciprocal of that value, times 100)
#      A3, the consumption of wine per capita,
#      A4, the consumption of hard liquor per capita,
#      B,  the death rate from cirrhosis.
#
#    We seek a model of the form:
#   
#      B = A0 * X0 + A1 * X1 + A2 * X2 + A3 * X3 + A4 * X4.
#    
7 columns
46 rows
Index
One
Urban population (percentage)
Late births (reciprocal * 100)
Wine consumption per capita
Liquor consumption per capita
Cirrhosis death rate
 1  1  44  33.2   5   30   41.2
 2  1  43  33.8   4   41   31.7
 3  1  48  40.6   3   38   39.4
 4  1  52  39.2   7   48   57.5
 5  1  71  45.5  11   53   74.8
 6  1  44  37.5   9   65   59.8
 7  1  57  44.2   6   73   54.3
 8  1  34  31.9   3   32   47.9
 9  1  70  45.6  12   56   77.2
10  1  54  45.9   7   57   56.6
11  1  70  43.7  14   43   80.9
12  1  65  32.1  12   33   34.3
13  1  36  36.9  10   48   53.1
14  1  47  38.9  10   69   55.4
15  1  63  47.6  14   54   57.8
16  1  35  33.0   9   47   62.8
17  1  50  38.9   7   68   67.3
18  1  55  35.7  18   47   56.7
19  1  33  31.2   6   27   37.6
20  1  81  53.8  31   79  129.9
21  1  63  42.5  13   59   70.3
22  1  78  53.3  20   97  104.2
23  1  63  47.0  19   95   83.6
24  1  65  44.9  10   81   66.0
25  1  45  35.6   4   26   52.3
26  1  78  50.5  16   76   86.9
27  1  60  42.3   9   37   66.6
28  1  52  43.8   6   46   40.1
29  1  37  33.2   6   40   55.7
30  1  55  36.0  21   76   58.1
31  1  69  47.6  15   70   74.3
32  1  84  50.0  17   66   98.1
33  1  54  43.8   7   63   40.7
34  1  61  45.0  13   59   66.7
35  1  47  42.2   8   55   48.0
36  1  57  53.0  28  149  122.5
37  1  87  51.6  23   77   92.1
38  1  50  31.9  22   43   76.0
39  1  85  56.1  23   74   97.5
40  1  27  31.5   7   56   33.8
41  1  84  50.0  16   63   90.5
42  1  37  32.4   2   41   29.7
43  1  33  36.1   6   59   28.0
44  1  44  35.3   3   32   51.6
45  1  63  39.3   8   40   55.7
46  1  58  43.8  13   57   55.5
