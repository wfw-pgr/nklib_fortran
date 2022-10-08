import sys
import numpy                      as np
import nkUtilities.load__config   as lcf
import nkUtilities.cMapTri        as cmt
import nkUtilities.configSettings as cfs


# ========================================================= #
# ===  display                                          === #
# ========================================================= #
def display():

    x_, y_, f_, a_, fx_, fy_ = 0, 1, 2, 3, 4, 5
    
    # ------------------------------------------------- #
    # --- [1] Arguments                             --- #
    # ------------------------------------------------- #
    config   = lcf.load__config()
    datFile1 = "dat/reference2d.dat"
    pngFile1 = "png/reference2d.png"
    datFile2 = "dat/interpolated2d.dat"
    pngFile2 = "png/interpolated2d.png"
    pngFile3 = "png/answer2d.png"
    pngFile4 = "png/dfdx2d.png"
    pngFile5 = "png/dfdy2d.png"
    
    # ------------------------------------------------- #
    # --- [2] Fetch Data                            --- #
    # ------------------------------------------------- #
    import nkUtilities.load__pointFile as lpf
    Data1  = lpf.load__pointFile( inpFile=datFile1, returnType="point" )
    Data2  = lpf.load__pointFile( inpFile=datFile2, returnType="point" )
    
    # ------------------------------------------------- #
    # --- [3] config Settings                       --- #
    # ------------------------------------------------- #
    config["FigSize"]        = (6,6)
    config["cmp_position"]   = [0.16,0.12,0.97,0.88]
    config["cmp_AutoLevel"]  = False
    config["cmp_MaxMin"]     = [ -1, +1 ]
    config["xTitle"]         = "X (m)"
    config["yTitle"]         = "Y (m)"
    config["cmp_xAutoRange"] = True
    config["cmp_yAutoRange"] = True
    config["cmp_xRange"]     = [-5.0,+5.0]
    config["cmp_yRange"]     = [-5.0,+5.0]

    # ------------------------------------------------- #
    # --- [4] plot Figure                           --- #
    # ------------------------------------------------- #
    cmt.cMapTri( xAxis=Data1[:,x_], yAxis=Data1[:,y_], cMap=Data1[:,f_], \
    		 pngFile=pngFile1, config=config )
    cmt.cMapTri( xAxis=Data2[:,x_], yAxis=Data2[:,y_], cMap=Data2[:,f_], \
    		 pngFile=pngFile2, config=config )
    cmt.cMapTri( xAxis=Data2[:,x_], yAxis=Data2[:,y_], cMap=Data2[:,a_], \
    		 pngFile=pngFile3, config=config )
    cmt.cMapTri( xAxis=Data2[:,x_], yAxis=Data2[:,y_], cMap=Data2[:,fx_], \
    		 pngFile=pngFile4, config=config )
    cmt.cMapTri( xAxis=Data2[:,x_], yAxis=Data2[:,y_], cMap=Data2[:,fy_], \
    		 pngFile=pngFile5, config=config )

# ======================================== #
# ===  実行部                          === #
# ======================================== #
if ( __name__=="__main__" ):
    display()

