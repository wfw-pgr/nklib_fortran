
import numpy as np
import os, sys

# ========================================================= #
# ===  display__distribution.py                         === #
# ========================================================= #

def display__distribution():

    i_,d1_,d2_,d3_,d4_       = 0, 1, 2, 3, 4
    inpFile                  = "dat/multi_grnd.dat"
    pngFile                  = "png/distribution.png"
    markersize               = 0.6
    
    # ------------------------------------------------- #
    # --- [1] load Data                             --- #
    # ------------------------------------------------- #
    import nkUtilities.load__pointFile as lpf
    Data    = lpf.load__pointFile( inpFile=inpFile, returnType="point" )
    Data    = Data[:10000]

    # ------------------------------------------------- #
    # --- [3] diplay                                --- #
    # ------------------------------------------------- #
    import nkUtilities.plot1D         as pl1
    import nkUtilities.load__config   as lcf
    import nkUtilities.configSettings as cfs
    config                   = lcf.load__config()
    config                   = cfs.configSettings( configType="plot.def", config=config )
    config["plt_xAutoRange"] = False
    config["plt_yAutoRange"] = False
    config["plt_xRange"]     = [ -0.1, +1.1 ]
    config["plt_yRange"]     = [ -0.1, +1.1 ]
    config["xMajor_auto"]    = False
    config["yMajor_auto"]    = False
    config["xMajor_ticks"]   = np.linspace( 0, 1, 6)
    config["yMajor_ticks"]   = np.linspace( 0, 1, 6)
    
    fig     = pl1.plot1D( config=config, pngFile=pngFile )
    fig.add__plot( xAxis=Data[:,d1_], yAxis=Data[:,d2_], \
                   linestyle="none", marker="o", markersize=markersize )
    fig.add__plot( xAxis=Data[:,d3_], yAxis=Data[:,d4_], \
                   linestyle="none", marker="o", markersize=markersize )
    fig.add__legend()
    fig.set__axis()
    fig.save__figure()


# ========================================================= #
# ===   Execution of Pragram                            === #
# ========================================================= #
if ( __name__=="__main__" ):
    display__distribution()
