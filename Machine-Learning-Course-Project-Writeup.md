Background
----------

Using devices such as Jawbone Up, Nike FuelBand, and Fitbit it is now
possible to collect a large amount of data about personal activity
relatively inexpensively. These type of devices are part of the
quantified self movement - a group of enthusiasts who take measurements
about themselves regularly to improve their health, to find patterns in
their behavior, or because they are tech geeks. One thing that people
regularly do is quantify how much of a particular activity they do, but
they rarely quantify how well they do it. In this project, your goal
will be to use data from accelerometers on the belt, forearm, arm, and
dumbell of 6 participants. They were asked to perform barbell lifts
correctly and incorrectly in 5 different ways. More information is
available from the website here:
<a href="http://groupware.les.inf.puc-rio.br/har" class="uri">http://groupware.les.inf.puc-rio.br/har</a>
(see the section on the Weight Lifting Exercise Dataset).

Approach
--------

Our outcome variable is classe, a factor variable. For this data set,
“participants were asked to perform one set of 10 repetitions of the
Unilateral Dumbbell Biceps Curl in 5 different fashions: - exactly
according to the specification (Class A) - throwing the elbows to the
front (Class B) - lifting the dumbbell only halfway (Class C) - lowering
the dumbbell only halfway (Class D) - throwing the hips to the front
(Class E)

Class A corresponds to the specified execution of the exercise, while
the other 4 classes correspond to common mistakes.Prediction evaluations
will be based on maximizing the accuracy and minimizing the
out-of-sample error. All other available variables after cleaning will
be used for prediction. Two models will be tested using decision tree
and random forest. The model with the highest accuracy will be chosen as
our final model.

Cross-validation
----------------

Cross-validation will be performed by subsampling our training data set
randomly without replacement into 2 subsamples: TrainTrainingSet data
(75% of the original Training data set) and TestTrainingSet data (25%).
Our models will be fitted on the TrainTrainingSet data set, and tested
on the TestTrainingSet data. Once the most accurate model is choosen, it
will be finally tested on the original Testing data set.

Expected out-of-sample error
----------------------------

The expected out-of-sample error will correspond to the quantity:
1-accuracy in the cross-validation data. Accuracy is the proportion of
correct classified observation over the total sample in the
TestTrainingSet data set. Expected accuracy is the expected accuracy in
the out-of-sample data set (i.e. original testing data set). Thus, the
expected value of the out-of-sample error will correspond to the
expected number of missclassified observations/total observations in the
Test data set, which is the quantity: 1-accuracy found from the
cross-validation data set.

Results
-------

Our outcome variable “classe” is a factor variable. We split the
Training dataset into TrainTrainingSet and TestTrainingSet datasets.

*Install packages, load the required libraries and set seed for
reproducibility.*

    library(lattice); library(ggplot2); library(caret); library(randomForest); library(rpart); library(rpart.plot); library(e1071)

    ## randomForest 4.6-14

    ## Type rfNews() to see new features/changes/bug fixes.

    ## 
    ## Attaching package: 'randomForest'

    ## The following object is masked from 'package:ggplot2':
    ## 
    ##     margin

    set.seed(1234)

*Load the data sets into R and make sure that missing values are coded
correctly.* *Delete irrelevant variables.*

    # Loading the training data set into my R session replacing all missing with "NA"
    trainingset <- read.csv("pml-training.csv", na.strings=c("NA","#DIV/0!", ""))
    # Loading the testing data set 
    testingset <- read.csv("pml-testing.csv", na.strings=c("NA","#DIV/0!", ""))
    # Check dimensions for number of variables and number of observations
    dim(trainingset)

    ## [1] 19622   160

    dim(testingset)

    ## [1]  20 160

    # Delete columns with all missing values
    trainingset<-trainingset[,colSums(is.na(trainingset)) == 0]
    testingset <-testingset[,colSums(is.na(testingset)) == 0]
    # Some variables are irrelevant to our current project: user_name, raw_timestamp_part_1, raw_timestamp_part_,2 cvtd_timestamp, new_window, and  num_window (columns 1 to 7). We can delete these variables.
    trainingset   <-trainingset[,-c(1:7)]
    testingset <-testingset[,-c(1:7)]
    # and have a look at our new datasets:
    dim(trainingset)

    ## [1] 19622    53

    dim(testingset)

    ## [1] 20 53

    head(trainingset)

    ##   roll_belt pitch_belt yaw_belt total_accel_belt gyros_belt_x gyros_belt_y
    ## 1      1.41       8.07    -94.4                3         0.00         0.00
    ## 2      1.41       8.07    -94.4                3         0.02         0.00
    ## 3      1.42       8.07    -94.4                3         0.00         0.00
    ## 4      1.48       8.05    -94.4                3         0.02         0.00
    ## 5      1.48       8.07    -94.4                3         0.02         0.02
    ## 6      1.45       8.06    -94.4                3         0.02         0.00
    ##   gyros_belt_z accel_belt_x accel_belt_y accel_belt_z magnet_belt_x
    ## 1        -0.02          -21            4           22            -3
    ## 2        -0.02          -22            4           22            -7
    ## 3        -0.02          -20            5           23            -2
    ## 4        -0.03          -22            3           21            -6
    ## 5        -0.02          -21            2           24            -6
    ## 6        -0.02          -21            4           21             0
    ##   magnet_belt_y magnet_belt_z roll_arm pitch_arm yaw_arm total_accel_arm
    ## 1           599          -313     -128      22.5    -161              34
    ## 2           608          -311     -128      22.5    -161              34
    ## 3           600          -305     -128      22.5    -161              34
    ## 4           604          -310     -128      22.1    -161              34
    ## 5           600          -302     -128      22.1    -161              34
    ## 6           603          -312     -128      22.0    -161              34
    ##   gyros_arm_x gyros_arm_y gyros_arm_z accel_arm_x accel_arm_y accel_arm_z
    ## 1        0.00        0.00       -0.02        -288         109        -123
    ## 2        0.02       -0.02       -0.02        -290         110        -125
    ## 3        0.02       -0.02       -0.02        -289         110        -126
    ## 4        0.02       -0.03        0.02        -289         111        -123
    ## 5        0.00       -0.03        0.00        -289         111        -123
    ## 6        0.02       -0.03        0.00        -289         111        -122
    ##   magnet_arm_x magnet_arm_y magnet_arm_z roll_dumbbell pitch_dumbbell
    ## 1         -368          337          516      13.05217      -70.49400
    ## 2         -369          337          513      13.13074      -70.63751
    ## 3         -368          344          513      12.85075      -70.27812
    ## 4         -372          344          512      13.43120      -70.39379
    ## 5         -374          337          506      13.37872      -70.42856
    ## 6         -369          342          513      13.38246      -70.81759
    ##   yaw_dumbbell total_accel_dumbbell gyros_dumbbell_x gyros_dumbbell_y
    ## 1    -84.87394                   37                0            -0.02
    ## 2    -84.71065                   37                0            -0.02
    ## 3    -85.14078                   37                0            -0.02
    ## 4    -84.87363                   37                0            -0.02
    ## 5    -84.85306                   37                0            -0.02
    ## 6    -84.46500                   37                0            -0.02
    ##   gyros_dumbbell_z accel_dumbbell_x accel_dumbbell_y accel_dumbbell_z
    ## 1             0.00             -234               47             -271
    ## 2             0.00             -233               47             -269
    ## 3             0.00             -232               46             -270
    ## 4            -0.02             -232               48             -269
    ## 5             0.00             -233               48             -270
    ## 6             0.00             -234               48             -269
    ##   magnet_dumbbell_x magnet_dumbbell_y magnet_dumbbell_z roll_forearm
    ## 1              -559               293               -65         28.4
    ## 2              -555               296               -64         28.3
    ## 3              -561               298               -63         28.3
    ## 4              -552               303               -60         28.1
    ## 5              -554               292               -68         28.0
    ## 6              -558               294               -66         27.9
    ##   pitch_forearm yaw_forearm total_accel_forearm gyros_forearm_x gyros_forearm_y
    ## 1         -63.9        -153                  36            0.03            0.00
    ## 2         -63.9        -153                  36            0.02            0.00
    ## 3         -63.9        -152                  36            0.03           -0.02
    ## 4         -63.9        -152                  36            0.02           -0.02
    ## 5         -63.9        -152                  36            0.02            0.00
    ## 6         -63.9        -152                  36            0.02           -0.02
    ##   gyros_forearm_z accel_forearm_x accel_forearm_y accel_forearm_z
    ## 1           -0.02             192             203            -215
    ## 2           -0.02             192             203            -216
    ## 3            0.00             196             204            -213
    ## 4            0.00             189             206            -214
    ## 5           -0.02             189             206            -214
    ## 6           -0.03             193             203            -215
    ##   magnet_forearm_x magnet_forearm_y magnet_forearm_z classe
    ## 1              -17              654              476      A
    ## 2              -18              661              473      A
    ## 3              -18              658              469      A
    ## 4              -16              658              469      A
    ## 5              -17              655              473      A
    ## 6               -9              660              478      A

    head(testingset)

    ##   roll_belt pitch_belt yaw_belt total_accel_belt gyros_belt_x gyros_belt_y
    ## 1    123.00      27.00    -4.75               20        -0.50        -0.02
    ## 2      1.02       4.87   -88.90                4        -0.06        -0.02
    ## 3      0.87       1.82   -88.50                5         0.05         0.02
    ## 4    125.00     -41.60   162.00               17         0.11         0.11
    ## 5      1.35       3.33   -88.60                3         0.03         0.02
    ## 6     -5.92       1.59   -87.70                4         0.10         0.05
    ##   gyros_belt_z accel_belt_x accel_belt_y accel_belt_z magnet_belt_x
    ## 1        -0.46          -38           69         -179           -13
    ## 2        -0.07          -13           11           39            43
    ## 3         0.03            1           -1           49            29
    ## 4        -0.16           46           45         -156           169
    ## 5         0.00           -8            4           27            33
    ## 6        -0.13          -11          -16           38            31
    ##   magnet_belt_y magnet_belt_z roll_arm pitch_arm yaw_arm total_accel_arm
    ## 1           581          -382     40.7    -27.80     178              10
    ## 2           636          -309      0.0      0.00       0              38
    ## 3           631          -312      0.0      0.00       0              44
    ## 4           608          -304   -109.0     55.00    -142              25
    ## 5           566          -418     76.1      2.76     102              29
    ## 6           638          -291      0.0      0.00       0              14
    ##   gyros_arm_x gyros_arm_y gyros_arm_z accel_arm_x accel_arm_y accel_arm_z
    ## 1       -1.65        0.48       -0.18          16          38          93
    ## 2       -1.17        0.85       -0.43        -290         215         -90
    ## 3        2.10       -1.36        1.13        -341         245         -87
    ## 4        0.22       -0.51        0.92        -238         -57           6
    ## 5       -1.96        0.79       -0.54        -197         200         -30
    ## 6        0.02        0.05       -0.07         -26         130         -19
    ##   magnet_arm_x magnet_arm_y magnet_arm_z roll_dumbbell pitch_dumbbell
    ## 1         -326          385          481     -17.73748       24.96085
    ## 2         -325          447          434      54.47761      -53.69758
    ## 3         -264          474          413      57.07031      -51.37303
    ## 4         -173          257          633      43.10927      -30.04885
    ## 5         -170          275          617    -101.38396      -53.43952
    ## 6          396          176          516      62.18750      -50.55595
    ##   yaw_dumbbell total_accel_dumbbell gyros_dumbbell_x gyros_dumbbell_y
    ## 1    126.23596                    9             0.64             0.06
    ## 2    -75.51480                   31             0.34             0.05
    ## 3    -75.20287                   29             0.39             0.14
    ## 4   -103.32003                   18             0.10            -0.02
    ## 5    -14.19542                    4             0.29            -0.47
    ## 6    -71.12063                   29            -0.59             0.80
    ##   gyros_dumbbell_z accel_dumbbell_x accel_dumbbell_y accel_dumbbell_z
    ## 1            -0.61               21              -15               81
    ## 2            -0.71             -153              155             -205
    ## 3            -0.34             -141              155             -196
    ## 4             0.05              -51               72             -148
    ## 5            -0.46              -18              -30               -5
    ## 6             1.10             -138              166             -186
    ##   magnet_dumbbell_x magnet_dumbbell_y magnet_dumbbell_z roll_forearm
    ## 1               523              -528               -56          141
    ## 2              -502               388               -36          109
    ## 3              -506               349                41          131
    ## 4              -576               238                53            0
    ## 5              -424               252               312         -176
    ## 6              -543               262                96          150
    ##   pitch_forearm yaw_forearm total_accel_forearm gyros_forearm_x gyros_forearm_y
    ## 1         49.30       156.0                  33            0.74           -3.34
    ## 2        -17.60       106.0                  39            1.12           -2.78
    ## 3        -32.60        93.0                  34            0.18           -0.79
    ## 4          0.00         0.0                  43            1.38            0.69
    ## 5         -2.16       -47.9                  24           -0.75            3.10
    ## 6          1.46        89.7                  43           -0.88            4.26
    ##   gyros_forearm_z accel_forearm_x accel_forearm_y accel_forearm_z
    ## 1           -0.59            -110             267            -149
    ## 2           -0.18             212             297            -118
    ## 3            0.28             154             271            -129
    ## 4            1.80             -92             406             -39
    ## 5            0.80             131             -93             172
    ## 6            1.35             230             322            -144
    ##   magnet_forearm_x magnet_forearm_y magnet_forearm_z problem_id
    ## 1             -714              419              617          1
    ## 2             -237              791              873          2
    ## 3              -51              698              783          3
    ## 4             -233              783              521          4
    ## 5              375             -787               91          5
    ## 6             -300              800              884          6

*Partitioning the training data set to allow cross-validation*

The training data set contains 53 variables and 19622 obs. The testing
data set contains 53 variables and 20 obs. In order to perform
cross-validation, the training data set is partionned into 2 sets:
subTraining (75%) and subTest (25%). This will be performed using random
subsampling without replacement.

    subsamples <- createDataPartition(y=trainingset$classe, p=0.75, list=FALSE)
    subTraining <- trainingset[subsamples, ] 
    subTesting <- trainingset[-subsamples, ]
    dim(subTraining)

    ## [1] 14718    53

    dim(subTesting)

    ## [1] 4904   53

    head(subTraining)

    ##    roll_belt pitch_belt yaw_belt total_accel_belt gyros_belt_x gyros_belt_y
    ## 3       1.42       8.07    -94.4                3         0.00         0.00
    ## 4       1.48       8.05    -94.4                3         0.02         0.00
    ## 5       1.48       8.07    -94.4                3         0.02         0.02
    ## 7       1.42       8.09    -94.4                3         0.02         0.00
    ## 9       1.43       8.16    -94.4                3         0.02         0.00
    ## 10      1.45       8.17    -94.4                3         0.03         0.00
    ##    gyros_belt_z accel_belt_x accel_belt_y accel_belt_z magnet_belt_x
    ## 3         -0.02          -20            5           23            -2
    ## 4         -0.03          -22            3           21            -6
    ## 5         -0.02          -21            2           24            -6
    ## 7         -0.02          -22            3           21            -4
    ## 9         -0.02          -20            2           24             1
    ## 10         0.00          -21            4           22            -3
    ##    magnet_belt_y magnet_belt_z roll_arm pitch_arm yaw_arm total_accel_arm
    ## 3            600          -305     -128      22.5    -161              34
    ## 4            604          -310     -128      22.1    -161              34
    ## 5            600          -302     -128      22.1    -161              34
    ## 7            599          -311     -128      21.9    -161              34
    ## 9            602          -312     -128      21.7    -161              34
    ## 10           609          -308     -128      21.6    -161              34
    ##    gyros_arm_x gyros_arm_y gyros_arm_z accel_arm_x accel_arm_y accel_arm_z
    ## 3         0.02       -0.02       -0.02        -289         110        -126
    ## 4         0.02       -0.03        0.02        -289         111        -123
    ## 5         0.00       -0.03        0.00        -289         111        -123
    ## 7         0.00       -0.03        0.00        -289         111        -125
    ## 9         0.02       -0.03       -0.02        -288         109        -122
    ## 10        0.02       -0.03       -0.02        -288         110        -124
    ##    magnet_arm_x magnet_arm_y magnet_arm_z roll_dumbbell pitch_dumbbell
    ## 3          -368          344          513      12.85075      -70.27812
    ## 4          -372          344          512      13.43120      -70.39379
    ## 5          -374          337          506      13.37872      -70.42856
    ## 7          -373          336          509      13.12695      -70.24757
    ## 9          -369          341          518      13.15463      -70.42520
    ## 10         -376          334          516      13.33034      -70.85059
    ##    yaw_dumbbell total_accel_dumbbell gyros_dumbbell_x gyros_dumbbell_y
    ## 3     -85.14078                   37                0            -0.02
    ## 4     -84.87363                   37                0            -0.02
    ## 5     -84.85306                   37                0            -0.02
    ## 7     -85.09961                   37                0            -0.02
    ## 9     -84.91563                   37                0            -0.02
    ## 10    -84.44602                   37                0            -0.02
    ##    gyros_dumbbell_z accel_dumbbell_x accel_dumbbell_y accel_dumbbell_z
    ## 3              0.00             -232               46             -270
    ## 4             -0.02             -232               48             -269
    ## 5              0.00             -233               48             -270
    ## 7              0.00             -232               47             -270
    ## 9              0.00             -232               47             -269
    ## 10             0.00             -235               48             -270
    ##    magnet_dumbbell_x magnet_dumbbell_y magnet_dumbbell_z roll_forearm
    ## 3               -561               298               -63         28.3
    ## 4               -552               303               -60         28.1
    ## 5               -554               292               -68         28.0
    ## 7               -551               295               -70         27.9
    ## 9               -549               292               -65         27.7
    ## 10              -558               291               -69         27.7
    ##    pitch_forearm yaw_forearm total_accel_forearm gyros_forearm_x
    ## 3          -63.9        -152                  36            0.03
    ## 4          -63.9        -152                  36            0.02
    ## 5          -63.9        -152                  36            0.02
    ## 7          -63.9        -152                  36            0.02
    ## 9          -63.8        -152                  36            0.03
    ## 10         -63.8        -152                  36            0.02
    ##    gyros_forearm_y gyros_forearm_z accel_forearm_x accel_forearm_y
    ## 3            -0.02            0.00             196             204
    ## 4            -0.02            0.00             189             206
    ## 5             0.00           -0.02             189             206
    ## 7             0.00           -0.02             195             205
    ## 9             0.00           -0.02             193             204
    ## 10            0.00           -0.02             190             205
    ##    accel_forearm_z magnet_forearm_x magnet_forearm_y magnet_forearm_z classe
    ## 3             -213              -18              658              469      A
    ## 4             -214              -16              658              469      A
    ## 5             -214              -17              655              473      A
    ## 7             -215              -18              659              470      A
    ## 9             -214              -16              653              476      A
    ## 10            -215              -22              656              473      A

    head(subTesting)

    ##    roll_belt pitch_belt yaw_belt total_accel_belt gyros_belt_x gyros_belt_y
    ## 1       1.41       8.07    -94.4                3         0.00         0.00
    ## 2       1.41       8.07    -94.4                3         0.02         0.00
    ## 6       1.45       8.06    -94.4                3         0.02         0.00
    ## 8       1.42       8.13    -94.4                3         0.02         0.00
    ## 12      1.43       8.18    -94.4                3         0.02         0.00
    ## 18      1.55       8.08    -94.4                3         0.00         0.02
    ##    gyros_belt_z accel_belt_x accel_belt_y accel_belt_z magnet_belt_x
    ## 1         -0.02          -21            4           22            -3
    ## 2         -0.02          -22            4           22            -7
    ## 6         -0.02          -21            4           21             0
    ## 8         -0.02          -22            4           21            -2
    ## 12        -0.02          -22            2           23            -2
    ## 18         0.00          -21            5           21             1
    ##    magnet_belt_y magnet_belt_z roll_arm pitch_arm yaw_arm total_accel_arm
    ## 1            599          -313     -128      22.5    -161              34
    ## 2            608          -311     -128      22.5    -161              34
    ## 6            603          -312     -128      22.0    -161              34
    ## 8            603          -313     -128      21.8    -161              34
    ## 12           602          -319     -128      21.5    -161              34
    ## 18           600          -316     -129      21.2    -161              34
    ##    gyros_arm_x gyros_arm_y gyros_arm_z accel_arm_x accel_arm_y accel_arm_z
    ## 1         0.00        0.00       -0.02        -288         109        -123
    ## 2         0.02       -0.02       -0.02        -290         110        -125
    ## 6         0.02       -0.03        0.00        -289         111        -122
    ## 8         0.02       -0.02        0.00        -289         111        -124
    ## 12        0.02       -0.03        0.00        -288         111        -123
    ## 18        0.02       -0.02       -0.03        -288         108        -124
    ##    magnet_arm_x magnet_arm_y magnet_arm_z roll_dumbbell pitch_dumbbell
    ## 1          -368          337          516      13.05217      -70.49400
    ## 2          -369          337          513      13.13074      -70.63751
    ## 6          -369          342          513      13.38246      -70.81759
    ## 8          -372          338          510      12.75083      -70.34768
    ## 12         -363          343          520      13.10321      -70.45975
    ## 18         -373          336          510      13.20646      -70.39037
    ##    yaw_dumbbell total_accel_dumbbell gyros_dumbbell_x gyros_dumbbell_y
    ## 1     -84.87394                   37             0.00            -0.02
    ## 2     -84.71065                   37             0.00            -0.02
    ## 6     -84.46500                   37             0.00            -0.02
    ## 8     -85.09708                   37             0.00            -0.02
    ## 12    -84.89472                   37             0.00            -0.02
    ## 18    -84.93667                   36             0.02            -0.02
    ##    gyros_dumbbell_z accel_dumbbell_x accel_dumbbell_y accel_dumbbell_z
    ## 1              0.00             -234               47             -271
    ## 2              0.00             -233               47             -269
    ## 6              0.00             -234               48             -269
    ## 8              0.00             -234               46             -272
    ## 12             0.00             -233               47             -270
    ## 18            -0.02             -231               47             -268
    ##    magnet_dumbbell_x magnet_dumbbell_y magnet_dumbbell_z roll_forearm
    ## 1               -559               293               -65         28.4
    ## 2               -555               296               -64         28.3
    ## 6               -558               294               -66         27.9
    ## 8               -555               300               -74         27.8
    ## 12              -554               291               -65         27.5
    ## 18              -557               292               -62         27.0
    ##    pitch_forearm yaw_forearm total_accel_forearm gyros_forearm_x
    ## 1          -63.9        -153                  36            0.03
    ## 2          -63.9        -153                  36            0.02
    ## 6          -63.9        -152                  36            0.02
    ## 8          -63.8        -152                  36            0.02
    ## 12         -63.8        -152                  36            0.02
    ## 18         -64.0        -151                  36            0.02
    ##    gyros_forearm_y gyros_forearm_z accel_forearm_x accel_forearm_y
    ## 1             0.00           -0.02             192             203
    ## 2             0.00           -0.02             192             203
    ## 6            -0.02           -0.03             193             203
    ## 8            -0.02            0.00             193             205
    ## 12            0.02           -0.03             191             203
    ## 18            0.00           -0.02             192             206
    ##    accel_forearm_z magnet_forearm_x magnet_forearm_y magnet_forearm_z classe
    ## 1             -215              -17              654              476      A
    ## 2             -216              -18              661              473      A
    ## 6             -215               -9              660              478      A
    ## 8             -213               -9              660              474      A
    ## 12            -215              -11              657              478      A
    ## 18            -216              -16              653              472      A

The variable “classe” contains 5 levels: A, B, C, D and E. A plot of the
outcome variable will allow us to see the frequency of each levels in
the TrainTrainingSet data set and \# compare one another.

    plot(subTraining$classe, col="pink", main="Plot of levels of variable classe within the TrainTrainingSet data set", xlab="classe", ylab="Frequency")

![](Machine-Learning-Course-Project-Writeup_files/figure-markdown_strict/unnamed-chunk-4-1.png)

Based on the graph above, we can see that each level frequency is within
the same order of magnitude of each other. Level A is the most frequent
while level D is the least frequent.

Prediction model 1: Decision Tree
---------------------------------

    model1 <- rpart(classe ~ ., data=subTraining, method="class")

    # Predicting:
    prediction1 <- predict(model1, subTesting, type = "class")

    # Plot of the Decision Tree
    rpart.plot(model1, main="Classification Tree", extra=102, under=TRUE, faclen=0)

![](Machine-Learning-Course-Project-Writeup_files/figure-markdown_strict/unnamed-chunk-5-1.png)

    # Test results on our TestTrainingSet data set:
    confusionMatrix(prediction1, subTesting$classe)

    ## Confusion Matrix and Statistics
    ## 
    ##           Reference
    ## Prediction    A    B    C    D    E
    ##          A 1251  149   15   61   17
    ##          B   38  572   75   60   75
    ##          C   39  117  696  117  122
    ##          D   49   58   51  508   58
    ##          E   18   53   18   58  629
    ## 
    ## Overall Statistics
    ##                                           
    ##                Accuracy : 0.7455          
    ##                  95% CI : (0.7331, 0.7577)
    ##     No Information Rate : 0.2845          
    ##     P-Value [Acc > NIR] : < 2.2e-16       
    ##                                           
    ##                   Kappa : 0.6774          
    ##                                           
    ##  Mcnemar's Test P-Value : < 2.2e-16       
    ## 
    ## Statistics by Class:
    ## 
    ##                      Class: A Class: B Class: C Class: D Class: E
    ## Sensitivity            0.8968   0.6027   0.8140   0.6318   0.6981
    ## Specificity            0.9310   0.9373   0.9024   0.9473   0.9633
    ## Pos Pred Value         0.8379   0.6976   0.6379   0.7017   0.8106
    ## Neg Pred Value         0.9578   0.9077   0.9583   0.9292   0.9341
    ## Prevalence             0.2845   0.1935   0.1743   0.1639   0.1837
    ## Detection Rate         0.2551   0.1166   0.1419   0.1036   0.1283
    ## Detection Prevalence   0.3044   0.1672   0.2225   0.1476   0.1582
    ## Balanced Accuracy      0.9139   0.7700   0.8582   0.7896   0.8307

Prediction model 2: Random Forest
---------------------------------

    model2 <- randomForest(classe ~. , data=subTraining, method="class")

    # Predicting:
    prediction2 <- predict(model2, subTesting, type = "class")

    # Test results on TestTrainingSet data set:
    confusionMatrix(prediction2, subTesting$classe)

    ## Confusion Matrix and Statistics
    ## 
    ##           Reference
    ## Prediction    A    B    C    D    E
    ##          A 1395    4    0    0    0
    ##          B    0  944    8    0    0
    ##          C    0    1  847    6    0
    ##          D    0    0    0  798    1
    ##          E    0    0    0    0  900
    ## 
    ## Overall Statistics
    ##                                           
    ##                Accuracy : 0.9959          
    ##                  95% CI : (0.9937, 0.9975)
    ##     No Information Rate : 0.2845          
    ##     P-Value [Acc > NIR] : < 2.2e-16       
    ##                                           
    ##                   Kappa : 0.9948          
    ##                                           
    ##  Mcnemar's Test P-Value : NA              
    ## 
    ## Statistics by Class:
    ## 
    ##                      Class: A Class: B Class: C Class: D Class: E
    ## Sensitivity            1.0000   0.9947   0.9906   0.9925   0.9989
    ## Specificity            0.9989   0.9980   0.9983   0.9998   1.0000
    ## Pos Pred Value         0.9971   0.9916   0.9918   0.9987   1.0000
    ## Neg Pred Value         1.0000   0.9987   0.9980   0.9985   0.9998
    ## Prevalence             0.2845   0.1935   0.1743   0.1639   0.1837
    ## Detection Rate         0.2845   0.1925   0.1727   0.1627   0.1835
    ## Detection Prevalence   0.2853   0.1941   0.1741   0.1629   0.1835
    ## Balanced Accuracy      0.9994   0.9964   0.9945   0.9961   0.9994

Decision - which prediction model to use?
-----------------------------------------

Random Forest algorithm performed better than Decision Trees.Accuracy
for Random Forest model was 0.995 (95% CI: (0.993, 0.997)) compared to
0.739 (95% CI: (0.727, 0.752)) for Decision Tree model. The random
Forest model is choosen. The accuracy of the model is 0.995. The
expected out-of-sample error is estimated at 0.005, or 0.5%. The
expected out-of-sample error is calculated as 1 - accuracy for
predictions made against the cross-validation set. Our Test data set
comprises 20 cases. With an accuracy above 99% on our cross-validation
data, we can expect that very few, or none, of the test samples will be
missclassified.

Submission
----------

    # predict outcome levels on the original Testing data set using Random Forest algorithm
    predictfinal <- predict(model2, testingset, type="class")
    predictfinal

    ##  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20 
    ##  B  A  B  A  A  E  D  B  A  A  B  C  B  A  E  E  A  B  B  B 
    ## Levels: A B C D E
