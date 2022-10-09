import sys
import numpy                      as np
import nkUtilities.load__config   as lcf
import nkUtilities.cMapTri        as cmt
import nkUtilities.configSettings as cfs


# ========================================================= #
# ===  display                                          === #
# ========================================================= #
def display():

    x_, y_, z_ = 0, 1, 2
    
    # ------------------------------------------------- #
    # --- [1] Arguments                             --- #
    # ------------------------------------------------- #
    config  = lcf.load__config()
    datFile = "dat/function2d.dat"
    ansFile = "dat/answer2d.dat"
    pngFile = "png/function2d.png"

    # ------------------------------------------------- #
    # --- [2] Fetch Data                            --- #
    # ------------------------------------------------- #
    import nkUtilities.load__pointFile as lpf
    Data  = lpf.load__pointFile( inpFile=datFile, returnType="point" )
    ans   = lpf.load__pointFile( inpFile=ansFile, returnType="point" )
    ans   = np.reshape( ans, (1,5) )
    
    # ------------------------------------------------- #
    # --- [3] config Settings                       --- #
    # ------------------------------------------------- #
    config                   = lcf.load__config()
    config["FigSize"]        = [6,6]
    config["cmp_position"]   = [0.16,0.16,0.90,0.86]
    config["xTitle"]         = "X (m)"
    config["yTitle"]         = "Y (m)"
    config["cmp_xAutoRange"] = True
    config["cmp_yAutoRange"] = True
    config["cmp_xRange"]     = [-1.0,+1.0]
    config["cmp_yRange"]     = [-1.0,+1.0]

    # ------------------------------------------------- #
    # --- [4] plot Figure                           --- #
    # ------------------------------------------------- #
    fig = cmt.cMapTri( pngFile=pngFile, config=config )
    fig.add__cMap( xAxis=Data[:,x_], yAxis=Data[:,y_], cMap=Data[:,z_] )
    fig.add__plot( xAxis=ans [:,x_], yAxis=ans [:,y_], \
                   marker="x", linestyle="none", color= "white",  )
    fig.set__axis()
    fig.save__figure()
    print( ans )


# ======================================== #
# ===  実行部                          === #
# ======================================== #
if ( __name__=="__main__" ):
    display()

