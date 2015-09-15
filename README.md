Pipelined Implementation of NORMA in CHISEL
===========================================

This code implements a fully pipelined version of NORMA, an O(n) machine learning algorithm used for classification, novelty detection or regression.

## Kernlab and Caret Benchmark Comparison

To run a full cross validation on a dataset with a CPU floating point implementation of NORMA first run the script `build.sh`
The script `runCVtests.R` will run a full cross validation on four datasets in this repo (takes a long time).
Edit this if you want to run on a different dataset

## Hardware NORMA Cross Validation

A cross validation using the hardware implementation can be run using the script `runChiselCV.R`.
It will run a full CV on emulated hardware in Chisel (takes a VERY long time).

## Params file format

The params file is a csv. The first row has 6 parameters:

| Parameter   | Description |
| ----------- | ----------- |
| `bitWidth`  | Number of bits to use for the fixed point representation |
| `fracWidth` | Of those bits how many are used for the fractional component |
| `log2Table` | The log2 of the number of values to use for the table with linear interpolation to compute the exponetial (4 -> 16 values) |
| `dictSize`  | Dictionary Size to use with NORMA |
| `features`  | The number of features in the dataset |
| `appType`   | The type of NORMA, 1 -> Classification, 2 -> Novelty detection, 3-> Regression |

The second row determines where to place registers in the pipeline. A TRUE enables that register where as FALSE just passes it through. The number of stages depends on the above parameters.

| Stage             | Number               | Description |
| ----------------- | -------------------- | ----------- |
| Kernel Evaluation | log2Up(features) + 7 | This stage computes the gaussian kernel function. It needs 7 operations with an adder tree using log2Up(features) operations. |
| Braiding Summation| variable             | This section computes the sum for the decision function. The number of stages in this section varies depending on how many are active. Assertions will ensure that the stages are valid so currently trial and error is the recommend method to do this section.
| Update and Output | 2                    | These last two stages must both be enabled or TRUE |

The third row has four parameters for NORMA when running with the emulator. They are:

| Parameter | Description |
| --------- | ----------- |
| `gamma`   | The parameter for the gaussian kernel = e^(-gamma*(x1 - x2)^2) with gamma > 0 |
| `forget`  | The parameter multiplied into the weights to forget the dictionary over time with 0 < forget < 1 |
| `eta`     | The correspond parameter in norma and initial weight when added with 0 < eta < 1. For classification and novelty detection forget = (1 - eta) |
| `nu`      | Affects the bias to determine how often examples are added to the dictionary with 0 < nu < 1 |

## Input dataset format

The input dataset has is also a CSV. Each row has the following

| Parameter | Description |
| --------- | ----------- |
| reset     | Not currently used, keep as False |
| forceNA   | Force the example to NOT be added to the dictionary and not affect the state of NORMA. Used to test a point without learning from it |
| Label/Class | The desired outcome to learn. For classification the labels should be 1 and -1, Novelty detection ignores this field, Regression expects this field to be a float which will be converted to a fixed |
| Features | An arbitrary number of features depending on the parameter above. They should be in float format and will then be converted to fixed |

# Running NORMA on the datasets

To run the NORMA algorithm on a dataset to obtain the performance on that dataset run:
`make normaRun PARAMSFILE=params_artificialTwoClass_Class.csv INPUTFILE=artificialTwoClass.csv OUTPUTFILE=test_out.csv`

This will compile and run norma with the parameters specified in the file `params_artificialTwoClass_Class.csv`
The same command can be used with other datasets.

To obtain the AUC and H performance of NORMA on the dataset, the R script `summarizeResults.R` is used.
After running the above command, run:
`Rscript summarizeResults.R --input=artificialTwoClass.csv --output=test_out.csv --print`

The parameters in the files were obtained by cross validating a floating point implementation.

# Generating the Verilog

To generate the verilog run:
`make normaVerilog PARAMSFILE=params_artificialTwoClass_Class.csv`
