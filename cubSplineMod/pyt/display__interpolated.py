import sys
import numpy                      as np
import nkUtilities.load__config   as lcf
import nkUtilities.plot1D         as pl1
import nkUtilities.configSettings as cfs


# ========================================================= #
# ===  display                                          === #
# ========================================================= #
def display():

    x_,y_,a_ = 0, 1, 2

    # ------------------------------------------------- #
    # --- [1] Arguments                             --- #
    # ------------------------------------------------- #
    config   = lcf.load__config()
    datFile1 = "dat/interpolated.dat"
    datFile2 = "dat/reference.dat"
    pngFile  = "png/interpolated.png"

    # ------------------------------------------------- #
    # --- [2] Fetch Data                            --- #
    # ------------------------------------------------- #
    import nkUtilities.load__pointFile as lpf
    Data1 = lpf.load__pointFile( inpFile=datFile1, returnType="point" )
    Data2 = lpf.load__pointFile( inpFile=datFile2, returnType="point" )
    
    # ------------------------------------------------- #
    # --- [3] config Settings                       --- #
    # ------------------------------------------------- #
    cfs.configSettings( configType="plot1D_def", config=config )
    config["xTitle"]         = "X"
    config["yTitle"]         = "Y"
    config["plt_xAutoRange"] = True
    config["plt_yAutoRange"] = True
    config["plt_xRange"]     = [-5.0,+5.0]
    config["plt_yRange"]     = [-5.0,+5.0]
    config["plt_linewidth"]  = 1.0
    config["xMajor_Nticks"]  = 11
    config["yMajor_Nticks"]  = 11

    # ------------------------------------------------- #
    # --- [4] plot Figure                           --- #
    # ------------------------------------------------- #
    fig = pl1.plot1D( config=config, pngFile=pngFile )
    fig.add__plot( xAxis=Data1[:,x_], yAxis=Data1[:,y_], label="intepolated", \
                   linestyle="-"   , color="royalBlue" )
    fig.add__plot( xAxis=Data1[:,x_], yAxis=Data1[:,a_], label="answer", \
                   linestyle="--"  , color="orange" )
    fig.add__plot( xAxis=Data2[:,x_], yAxis=Data2[:,y_], label="reference", \
                   linestyle="none", marker="o", color="Red", markersize=3.0 )
    fig.set__axis()
    fig.add__legend()
    fig.save__figure()


# ======================================== #
# ===  実行部                          === #
# ======================================== #
if ( __name__=="__main__" ):
    display()

