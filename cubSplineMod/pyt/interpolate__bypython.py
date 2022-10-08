import numpy as np
import os, sys
import nkInterpolator.interpolate__spline as spl


# ========================================================= #
# ===  interpolate__bypython.py                         === #
# ========================================================= #

def interpolate__bypython():

    x_, y_, a_ = 0, 1, 2
    
    # ------------------------------------------------- #
    # --- [1] load data                             --- #
    # ------------------------------------------------- #
    import nkUtilities.load__pointFile as lpf
    inpFile1 = "dat/reference.dat"
    inpFile2 = "dat/interpolated.dat"
    Data1    = lpf.load__pointFile( inpFile=inpFile1, returnType="point" )
    Data2    = lpf.load__pointFile( inpFile=inpFile2, returnType="point" )

    # ------------------------------------------------- #
    # --- [2] interpolate                           --- #
    # ------------------------------------------------- #
    xval = np.copy( Data2[:,0] )
    yval = spl.interpolate__spline( xval=xval, xref=Data1[:,0], yref=Data1[:,1] )

    # ------------------------------------------------- #
    # --- [3] plot                                  --- #
    # ------------------------------------------------- #
    import nkUtilities.plot1D         as pl1
    import nkUtilities.load__config   as lcf
    import nkUtilities.configSettings as cfs
    x_,y_                    = 0, 1
    pngFile                  = "png/interpolate_bypython.png"
    config                   = lcf.load__config()
    config                   = cfs.configSettings( configType="plot.def", config=config )
    config["plt_xAutoRange"] = True
    config["plt_yAutoRange"] = True
    config["plt_xRange"]     = [ -1.2, +1.2 ]
    config["plt_yRange"]     = [ -1.2, +1.2 ]
    fig     = pl1.plot1D( config=config, pngFile=pngFile )
    fig.add__plot( xAxis=Data1[:,x_], yAxis=Data1[:,y_], label="reference", color="Red", \
                   marker="o", markersize=3.0, linestyle="none" )
    fig.add__plot( xAxis=Data2[:,x_], yAxis=Data2[:,a_], label="answer"   , color="Green", \
                   linestyle="--", linewidth=0.8 )
    fig.add__plot( xAxis=xval       , yAxis=yval       , label="python"   , color="magenta"  , \
                   linestyle="none", marker="x", markersize=4.0 )
    fig.add__plot( xAxis=Data2[:,x_], yAxis=Data2[:,y_], label="fortran"  , color="royalBlue", \
                   linestyle="none", marker="+", markersize=4.0  )
    fig.add__legend()
    fig.set__axis()
    fig.save__figure()


    
    return()


# ========================================================= #
# ===   Execution of Pragram                            === #
# ========================================================= #

if ( __name__=="__main__" ):
    interpolate__bypython()

