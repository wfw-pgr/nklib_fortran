import sys
import numpy                      as np
import nkUtilities.load__config   as lcf
import nkUtilities.plot1D         as pl1
import nkUtilities.configSettings as cfs


# ========================================================= #
# ===  display                                          === #
# ========================================================= #
def display():

    x_,y_ = 0, 1

    # ------------------------------------------------- #
    # --- [1] Arguments                             --- #
    # ------------------------------------------------- #
    config   = lcf.load__config()
    datFile1 = "dat/function.dat"
    datFile2 = "dat/answer.dat"
    pngFile  = "png/function.png"

    # ------------------------------------------------- #
    # --- [2] Fetch Data                            --- #
    # ------------------------------------------------- #
    import nkUtilities.load__pointFile as lpf
    Data1 = lpf.load__pointFile( inpFile=datFile1, returnType="point" )
    Data2 = lpf.load__pointFile( inpFile=datFile2, returnType="point" )
    Data2 = np.reshape( Data2, (-1,2) )
        
    # ------------------------------------------------- #
    # --- [3] config Settings                       --- #
    # ------------------------------------------------- #
    cfs.configSettings( configType="plot1D_def", config=config )
    config["xTitle"]         = "X (m)"
    config["yTitle"]         = "Y (m)"
    config["plt_xAutoRange"] = True
    config["plt_yAutoRange"] = True
    config["plt_xRange"]     = [ 0.0, 7.0]
    config["plt_yRange"]     = [-1.2,+1.2]
    config["plt_linewidth"]  = 1.0
    config["xMajor_Nticks"]  = 8
    config["yMajor_Nticks"]  = 9

    # ------------------------------------------------- #
    # --- [4] plot Figure                           --- #
    # ------------------------------------------------- #
    fig = pl1.plot1D( config=config, pngFile=pngFile )
    fig.add__plot( xAxis=Data1[:,x_], yAxis=Data1[:,y_], color="RoyalBlue", \
                   linestyle="--", linewidth=1.0 )
    fig.add__plot( xAxis=Data2[:,0], yAxis=Data2[:,1]  , color="Red", \
                   linestyle="none", marker="o", markersize=2.0)
    fig.set__axis()
    fig.save__figure()


# ======================================== #
# ===  実行部                          === #
# ======================================== #
if ( __name__=="__main__" ):
    display()

