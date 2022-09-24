
import numpy as np
import os, sys

# ========================================================= #
# ===  display__seriesplot.py                           === #
# ========================================================= #

def display__seriesplot():

    i_                       = 0
    inpFile                  = "dat/grnd.dat"
    pngFile                  = "png/seriesplot.png"
    markersize               = 1.2
    
    # ------------------------------------------------- #
    # --- [1] load Data                             --- #
    # ------------------------------------------------- #
    import nkUtilities.load__pointFile as lpf
    Data    = lpf.load__pointFile( inpFile=inpFile, returnType="point" )
    labels  = [ "rand{0:02}".format(ik+1) for ik in list( range( Data.shape[1]-1 ) ) ]
    Data    = Data[:10000]
    
    # ------------------------------------------------- #
    # --- [3] diplay                                --- #
    # ------------------------------------------------- #
    import nkUtilities.plot1D         as pl1
    import nkUtilities.load__config   as lcf
    import nkUtilities.configSettings as cfs
    config                   = lcf.load__config()
    config                   = cfs.configSettings( configType="plot.def", config=config )
    config["plt_xAutoRange"] = True
    config["plt_yAutoRange"] = False
    config["plt_xRange"]     = [ -0.1, +1.1 ]
    config["plt_yRange"]     = [ -0.1, +1.1 ]
    config["xMajor_auto"]    = True
    config["yMajor_auto"]    = False
    config["xMajor_ticks"]   = np.linspace( 0, 1, 6)
    config["yMajor_ticks"]   = np.linspace( 0, 1, 6)
    
    fig     = pl1.plot1D( config=config, pngFile=pngFile )
    for ik in list( range( Data.shape[1]-1 ) ):
        fig.add__plot( xAxis=Data[:,i_], yAxis=Data[:,ik+1], label=labels[ik], \
                       linestyle="none", marker="o", markersize=markersize )
    fig.add__legend()
    fig.set__axis()
    fig.save__figure()


# ========================================================= #
# ===   Execution of Pragram                            === #
# ========================================================= #
if ( __name__=="__main__" ):
    display__seriesplot()
