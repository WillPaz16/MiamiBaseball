"""Some hopefully useful functions for MTH 433/533 Fall 2023
    at Miami University
"""
import numpy as np
import matplotlib.pyplot as plt
from scipy import linalg as la
import pandas as pd

def comp_df(dataframe):
    """
    Input:  dataframe with a 'Name' column
    Returns: A DataFrame with index the 'Name' column of dataframe, 4 columns labelled 'Comp1',...,'Comp4'
    and empty entries.  Meant to be populated with the names of the 4 closest comps.
    """
    names = dataframe['Name']
    m = len(names)
    A = np.empty((m,4),dtype=object)
    df = pd.DataFrame(A,columns=['Comp1','Comp2','Comp3','Comp4'],index = names)
    return df

def dist_mat(df):
    """
    Input:  dataframe with a 'Name' column
    Returns: A DataFrame with index and columns both set to the 'Name' column of dataframe.  All 
    other entries set to 0.  Meant to be populated with the Euclidean squared distance between
    the players first two principal components
    """
    names = list(df['Name'])
    m,_ = df.shape
    B = np.zeros((m,m))
    newdf = pd.DataFrame(B,columns=names,index=names)
    
    return newdf

#Stole this trick from stack exchange
def trunc(values, decs=0):
    """ Truncates an array to the specified number of decimal places
        Inputs: A matrix and the number of decimal places you want to
                truncate to
        Returns: The truncated matrix
    """
    return np.trunc(values*10**decs)/(10**decs)

def elem_mat(n,row,column,scalar):
    """
    Returns the elementary nxn matrix that has scalar in the
    row, column position
    Inputs: the size of the matrix you want, the row and column where you want to put the scalar
    Returns: The elementary nxn matrix that has scalar in the 
    row,column position
    """
    A = np.eye(n)
    A[row,column] = scalar
    return A

def row_add(A,row1,row2,scalar):
    """
    Performs the row operation of adding a scalar multiple of row1 to row2
    Inputs: the matrix A, row numbers and scalar
    Returns: the matrix after the row operation has been applied
    """
    n,_ = A.shape 
    #The underscore n,_ tells python not to calculate the column size. 
    #In some expensive calculations this could save computing time.
    #Here it is  helpful coding 
    #since it signifies you only care about the number of rows
    E = elem_mat(n,row2,row1,scalar)
    return E @ A

def row_swap(A,row1,row2):
    """
    Swaps two rows of a matrix
    Input: a matrix and the rows you want to swap
    Output: the resultant matrix after swapping rows
    """
    C = A[row1,:].copy() #if you don't make a copy it will change A
    A[row1,:] = A[row2,:]
    A[row2,:] = C
    return A

def row_reduce(A,decs=2):
    """
    Puts a matrix in row echelon form
    Input: a matrix A and possible decimal place argument. Default is 2.
    Output: An (truncated to decs decimal places) 
            upper triangular matrix row equivalent to A
    """
    row = 0 #current row
    column = 0 #current column
    n,m = A.shape 
    while row < n and column < m:
        #if the (row,column) entry is not zero then clear everything 
        #out below it and move to the right one column and down one row
        if not np.allclose(A[row,column],0):  
            for j in range(row + 1,n):
                A = row_add(A,row,j,-A[j,column]/A[row,column])
            row = row + 1
            column = column + 1
        #if all zeros are below the current position
        #keep the same row and move over one column
        elif np.allclose(A[row + 1:,column],np.zeros((n-row-1,1))):
            column = column + 1
        #If the pivot position is non-zero but there is 
        #non-zero entry below it then perform a row swap 
        #and go back to the top of the while loop
        else:
            for jj in range(row + 1,n):
                if not np.allclose(A[jj,column],0):
                    A = row_swap(A,row,jj)
                    break
    return trunc(A,decs)

#### Testing functions for LU_fact
def rand_lower_unt(n):
    """
    Testing function: makes a nxn lower unitriangular matrix
    with random integer entires
    Input: size of the matrix you want
    Return: A lower unitriangular matrix with random integer entries
            in the range [-5,5] below the diagonal
    """
    L = np.eye(n)
    for row in range(1,n):
        for column in range(row):
            L[row,column] = np.random.randint(-5,5)
    return L

def rand_upper_tri(n):
    """
    Testing function: makes an upper triangular matrix with non-zero
                      entries on the diagonal
    Input: the size of the matrix you want
    Return: An upper triangular nxn matrix with random non-zero diagonal
            entries
    """
    U = rand_lower_unt(n)
    for i in range(n):
        U[i,i] = np.random.randint(1,10)*(-1)**np.random.randint(0,2)
    return U.T

def rand_regular(n):
    """
    Testing function: Makes a random regular matrix
    Input: The size of the matrix you want
    Return: A regular nxn matrix
    """
    L = rand_lower_unt(n)
    U = rand_upper_tri(n)
    return L @ U

def rand_not_regular(n):
    """
    Testing function: Makes a random regular matrix
    Input: The size of the matrix you want
    Return: A regular nxn matrix
    """
    L = rand_lower_unt(n)
    U = rand_upper_tri(n)
    k = np.random.randint(1,n+1)
    for i in range(k):
        index = np.random.randint(0,n)
        U[index, index] = 0
    return L @ U

def LU_test_reg(n, function,low=1,high=40):
    """
    Testing function: generates n regular matrices of various sizes, feeds them through an LU
                      factorization function and tests if LU = the original matrix
    Input: an integer n n for how many matrices to generate and optional entries low,high
           that control the size of the test matrices and the name of your function probably
           LU_fact
    Returns: A report which is a list of 'problem matrices'.  One should use a problem
            tuple (A,L,U) to help debug
    """
    report = []
    for i in range(n):
        size = np.random.randint(low,high)
        A = rand_regular(size)
        _,L,U = function(A)
        if not np.allclose(A, L@U):
            report.append((A,L,U))
    print(f"Your function got {len(report)} regular matrices out of {n} wrong")
    return report

def LU_test_not_reg(n,function,low=1,high=40):
    """
    Testing function: generates n non-regular matrices of various sizes, feeds them through an LU
                      factorization function and tests if LU = the original matrix
    Input: an integer n n for how many matrices to generate and optional entries low,high
           that control the size of the test matrices
    Returns: A report which is a list of 'problem matrices'.  One should use a problem
            tuple (A,L,U) to help debug
    """
    report = []
    for i in range(n):
        size = np.random.randint(low,high)
        A = rand_not_regular(size)
        Bool,L,U = function(A)
        if Bool:
            report.append((A,L,U))
    print(f"Your function got {len(report)} non-regular matrices out of {n} wrong")
    return report

def LU_test(n,function,low=1,high=40):
    """
    Testing function: generates regular and non-regular matrices of various sizes, feeds them through an                         LU factorization function and tests if its correct
    Input: an integer n for how many matrices to generate and optional entries low,high
           that control the size of the test matrices and the name of your function probably
           LU_fact
    Returns: A report which is a list of 'problem matrices'.  One should use a problem
            tuple (A,L,U) to help debug
    """
    reg_report = LU_test_reg(n,function,low,high)
    non_reg_report = LU_test_not_reg(n,function,low,high)
    return reg_report,non_reg_report


def recordpts(n,show=False):
    """ Records and plots n points specified by the user. Default is to erase the points
        after the user clicks them.  Seems counterintuitive but will probably what we want
        when making splines.
        Inputs: An integer n
        Returns: A nx2 numpy array
    """
    plt.figure()   #opens an empty figure
    plt.xlim(0, 1) #Restricts the x-axis to [0,1]
    plt.ylim(0, 1) #Restricts the y-axis to [0,1]
    
    #ginput is how we get info from the user. 
    #https://matplotlib.org/stable/api/_as_gen/matplotlib.pyplot.ginput.html
    pts = np.asarray(plt.ginput(n))
    if show:
        plt.plot(pts[:,0],pts[:,1],'.') #Optional if you want to see the points
    
    return pts

def LU_fact(A):
    """
    Decides is a matrix is regular or not.  If regular it returns the LU factorizatiion
    Input: Square matrix
    Returns: A 3-tuple of a Boolean value and two matrices.  If the matrix is not regular the matrices
    are empty.
    """
    n,_ = A.shape
    L = np.eye(n) 
    for column in range(n):
        if np.allclose(A[column,column],0):
            return False, np.array([]), np.array([])
        else:
            for row in range(column + 1, n):
                L[row,column] = A[row,column]/A[column,column]
                A = row_add(A,column,row,-A[row,column]/A[column,column])
    return True, L, A

        
def PD_test(A):
    if not np.allclose(A,A.T):
        return False
    else:
        Bool,L,U = LU_fact(A)
        if not Bool:
            return False
        else: 
            _,n =  A.shape
            for i in range(n):
                if U[i,i] < 0:
                    return False
            return True
        
def induction_magic(B):
    _,E = np.linalg.eigh(B)
    return E
    
def init_compdf(dataframe):
    """
    Input:  dataframe with a 'Name' column
    Returns: A DataFrame with index the 'Name' column of dataframe, 4 columns labelled 'Comp1',...,'Comp4'
    and empty entries.  Meant to be populated with the names of the 4 closest comps.
    """
    names = dataframe['Name']
    m = len(names)
    A = np.empty((m,4),dtype=object)
    df = pd.DataFrame(A,columns=['Comp1','Comp2','Comp3','Comp4'],index = names)
    return df

def init_distdf(df):
    """
    Input:  dataframe with a 'Name' column
    Returns: A DataFrame with index and columns both set to the 'Name' column of dataframe.  All 
    other entries set to 0.  Meant to be populated with the Euclidean squared distance between
    the players first two principal components
    """
    names = list(df['Name'])
    m,_ = df.shape
    B = np.zeros((m,m))
    newdf = pd.DataFrame(B,columns=names,index=names)
    #newdf.insert(0,'Name',names)
    return newdf

