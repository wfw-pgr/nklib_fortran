import os, sys
import numpy as np
import nkUtilities.plot1D         as pl1
import nkUtilities.load__config   as lcf
import nkUtilities.configSettings as cfs


# ========================================================= #
# ===  display__hist.py                                 === #
# ========================================================= #

def display__hist():

    x_,y_        = 0, 1
    # inpFile      = "dat/grnd.dat"
    # inpFile      = "dat/gauss__dist.dat"
    inpFile      = "dat/gauss__multi.dat"
    pngFile      = "png/histogram_{:02}.png"
    histMinMax   = [ -5, +5 ]
    bins         = 100
    
    # ------------------------------------------------- #
    # --- [1] load data                             --- #
    # ------------------------------------------------- #
    import nkUtilities.load__pointFile as lpf
    Data    = lpf.load__pointFile( inpFile=inpFile, returnType="point" )
    nData   = Data.shape[1] - 1

    # ------------------------------------------------- #
    # --- [2] calculate histogram                   --- #
    # ------------------------------------------------- #
    stack = []
    for ik in range( nData ):
        hist,bins  = np.histogram( Data[:,ik+1], bins=bins, range=histMinMax, density=True )
        bins_c     = ( 0.5 * ( bins + np.roll( bins, -1 ) ) )[:-1]
        stack     += [ (bins_c, hist) ]
    
    # ------------------------------------------------- #
    # --- [3] display histogram                     --- #
    # ------------------------------------------------- #
    config                   = lcf.load__config()
    config                   = cfs.configSettings( configType="plot.def", config=config )
    config["plt_xAutoRange"] = False
    config["plt_yAutoRange"] = False
    config["plt_xRange"]     = [ -5, +5   ]
    config["plt_yRange"]     = [  0, +1.0 ]

    for ik in range( nData ):
        fig     = pl1.plot1D( config=config, pngFile=pngFile.format( ik+1 ) )
        bins_c, hist = stack[ik]
        fig.add__bar( xAxis=bins_c, yAxis=hist )
        fig.set__axis()
        fig.save__figure()


# ========================================================= #
# ===   Execution of Pragram                            === #
# ========================================================= #

if ( __name__=="__main__" ):
    display__hist()
