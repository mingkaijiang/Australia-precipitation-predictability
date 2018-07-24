Processing_data <- function(sourceDir, destDir) {
    #### To process the raw data into format easily readable
    
    for (i in 1930:2009) {
        ### complete the path
        sDir <- paste0(sourceDir, "/", i)

        ### create destDir if not exists
        if(!dir.exists(destDir)) {
            dir.create(destDir, showWarnings = FALSE)
        }
        
        ### Source all files in input folder
        DatFiles <- list.files(path = sDir, pattern = "\\.grid")
        
        ### Prepare output array
        if (leap_year(i)) {
            daily.tmp <- array(NA, c(691, 886, 366))
        } else {
            daily.tmp <- array(NA, c(691, 886, 365))
        }
        
        ### Prepare monthly output array
        out <- array(NA, c(691, 886, 12))
        
        ### Read in data
        for (j in 1:length(DatFiles)) {
            inName <- file.path(sDir, DatFiles[j], fsep = .Platform$file.sep)
            myDF <- read.ascii.grid(inName)
            
            daily.tmp[, , j] <- myDF$data
        }   
        
        ### calculate monthly sum precipitation for each grid
        ## Jan
        out[,,1] <- matrix(mapply(sum, daily.tmp[,,1],daily.tmp[,,2],daily.tmp[,,3],
                                  daily.tmp[,,4],daily.tmp[,,5],daily.tmp[,,6],
                                  daily.tmp[,,7],daily.tmp[,,8],daily.tmp[,,9],daily.tmp[,,10],
                                  daily.tmp[,,11],daily.tmp[,,12],daily.tmp[,,13],
                                  daily.tmp[,,14],daily.tmp[,,15],daily.tmp[,,16],
                                  daily.tmp[,,17],daily.tmp[,,18],daily.tmp[,,19],daily.tmp[,,20],
                                  daily.tmp[,,21],daily.tmp[,,22],daily.tmp[,,23],
                                  daily.tmp[,,24],daily.tmp[,,25],daily.tmp[,,26],
                                  daily.tmp[,,27],daily.tmp[,,28],daily.tmp[,,29],
                                  daily.tmp[,,30],daily.tmp[,,31],
                                  MoreArgs=list(na.rm=T)))
        
        if (leap_year(i)) {
            ## Feb   31 + 29 = 60
            out[,,2] <- matrix(mapply(sum, daily.tmp[,,32],daily.tmp[,,33],
                                      daily.tmp[,,34],daily.tmp[,,35],daily.tmp[,,36],
                                      daily.tmp[,,37],daily.tmp[,,38],daily.tmp[,,39],daily.tmp[,,40],
                                      daily.tmp[,,41],daily.tmp[,,42],daily.tmp[,,43],
                                      daily.tmp[,,44],daily.tmp[,,45],daily.tmp[,,46],
                                      daily.tmp[,,47],daily.tmp[,,48],daily.tmp[,,49],daily.tmp[,,50],
                                      daily.tmp[,,51],daily.tmp[,,52],daily.tmp[,,53],
                                      daily.tmp[,,54],daily.tmp[,,55],daily.tmp[,,56],
                                      daily.tmp[,,57],daily.tmp[,,58],daily.tmp[,,59],daily.tmp[,,60],
                                      MoreArgs=list(na.rm=T)))
            
            ## Mar    60 + 31 = 91
            out[,,3] <- matrix(mapply(sum, daily.tmp[,,61],daily.tmp[,,62],daily.tmp[,,63],
                                      daily.tmp[,,64],daily.tmp[,,65],daily.tmp[,,66],
                                      daily.tmp[,,67],daily.tmp[,,68],daily.tmp[,,69],daily.tmp[,,70],
                                      daily.tmp[,,71],daily.tmp[,,72],daily.tmp[,,73],
                                      daily.tmp[,,74],daily.tmp[,,75],daily.tmp[,,76],
                                      daily.tmp[,,77],daily.tmp[,,78],daily.tmp[,,79],daily.tmp[,,80],
                                      daily.tmp[,,81],daily.tmp[,,82],daily.tmp[,,83],
                                      daily.tmp[,,84],daily.tmp[,,85],daily.tmp[,,86],
                                      daily.tmp[,,87],daily.tmp[,,88],daily.tmp[,,89],daily.tmp[,,90],
                                      daily.tmp[,,91],
                                      MoreArgs=list(na.rm=T)))
            
            ## Apr    91 + 30 = 121
            out[,,4] <- matrix(mapply(sum, daily.tmp[,,92],daily.tmp[,,93],
                                      daily.tmp[,,94],daily.tmp[,,95],daily.tmp[,,96],
                                      daily.tmp[,,97],daily.tmp[,,98],daily.tmp[,,99],daily.tmp[,,100],
                                      daily.tmp[,,101],daily.tmp[,,102],daily.tmp[,,103],
                                      daily.tmp[,,104],daily.tmp[,,105],daily.tmp[,,106],
                                      daily.tmp[,,107],daily.tmp[,,108],daily.tmp[,,109],daily.tmp[,,110],
                                      daily.tmp[,,111],daily.tmp[,,112],daily.tmp[,,113],
                                      daily.tmp[,,114],daily.tmp[,,115],daily.tmp[,,116],
                                      daily.tmp[,,117],daily.tmp[,,118],daily.tmp[,,119],daily.tmp[,,120],
                                      daily.tmp[,,121],
                                      MoreArgs=list(na.rm=T)))
            
            ## May    121 + 31 = 152
            out[,,5] <- matrix(mapply(sum, daily.tmp[,,122],daily.tmp[,,123],
                                      daily.tmp[,,124],daily.tmp[,,125],daily.tmp[,,126],
                                      daily.tmp[,,127],daily.tmp[,,128],daily.tmp[,,129],daily.tmp[,,130],
                                      daily.tmp[,,131],daily.tmp[,,132],daily.tmp[,,133],
                                      daily.tmp[,,134],daily.tmp[,,135],daily.tmp[,,136],
                                      daily.tmp[,,137],daily.tmp[,,138],daily.tmp[,,139],daily.tmp[,,140],
                                      daily.tmp[,,141],daily.tmp[,,142],daily.tmp[,,143],
                                      daily.tmp[,,144],daily.tmp[,,145],daily.tmp[,,146],
                                      daily.tmp[,,147],daily.tmp[,,148],daily.tmp[,,149],daily.tmp[,,150],
                                      daily.tmp[,,151],daily.tmp[,,152],
                                      MoreArgs=list(na.rm=T)))
            
            ## Jun    152 + 30 = 182
            out[,,6] <- matrix(mapply(sum, daily.tmp[,,153],
                                      daily.tmp[,,154],daily.tmp[,,155],daily.tmp[,,156],
                                      daily.tmp[,,157],daily.tmp[,,158],daily.tmp[,,159],daily.tmp[,,160],
                                      daily.tmp[,,161],daily.tmp[,,162],daily.tmp[,,163],
                                      daily.tmp[,,164],daily.tmp[,,165],daily.tmp[,,166],
                                      daily.tmp[,,167],daily.tmp[,,168],daily.tmp[,,169],daily.tmp[,,170],
                                      daily.tmp[,,171],daily.tmp[,,172],daily.tmp[,,173],
                                      daily.tmp[,,174],daily.tmp[,,175],daily.tmp[,,176],
                                      daily.tmp[,,177],daily.tmp[,,178],daily.tmp[,,179],daily.tmp[,,180],
                                      daily.tmp[,,181],daily.tmp[,,182],
                                      MoreArgs=list(na.rm=T)))
            
            ## Jul    182 + 31 = 213
            out[,,7] <- matrix(mapply(sum, daily.tmp[,,183],
                                      daily.tmp[,,184],daily.tmp[,,185],daily.tmp[,,186],
                                      daily.tmp[,,187],daily.tmp[,,188],daily.tmp[,,189],daily.tmp[,,190],
                                      daily.tmp[,,191],daily.tmp[,,192],daily.tmp[,,193],
                                      daily.tmp[,,194],daily.tmp[,,195],daily.tmp[,,196],
                                      daily.tmp[,,197],daily.tmp[,,198],daily.tmp[,,199],daily.tmp[,,200],
                                      daily.tmp[,,201],daily.tmp[,,202],daily.tmp[,,203],
                                      daily.tmp[,,204],daily.tmp[,,205],daily.tmp[,,206],
                                      daily.tmp[,,207],daily.tmp[,,208],daily.tmp[,,209],daily.tmp[,,210],
                                      daily.tmp[,,211],daily.tmp[,,212],daily.tmp[,,213],
                                      MoreArgs=list(na.rm=T)))
            
            ## Aug    213 + 31 = 244
            out[,,8] <- matrix(mapply(sum, 
                                      daily.tmp[,,214],daily.tmp[,,215],daily.tmp[,,216],
                                      daily.tmp[,,217],daily.tmp[,,218],daily.tmp[,,219],daily.tmp[,,220],
                                      daily.tmp[,,221],daily.tmp[,,222],daily.tmp[,,223],
                                      daily.tmp[,,224],daily.tmp[,,225],daily.tmp[,,226],
                                      daily.tmp[,,227],daily.tmp[,,228],daily.tmp[,,229],daily.tmp[,,230],
                                      daily.tmp[,,231],daily.tmp[,,232],daily.tmp[,,233],
                                      daily.tmp[,,234],daily.tmp[,,235],daily.tmp[,,236],
                                      daily.tmp[,,237],daily.tmp[,,238],daily.tmp[,,239],daily.tmp[,,240],
                                      daily.tmp[,,241],daily.tmp[,,242],daily.tmp[,,243],daily.tmp[,,244],
                                      MoreArgs=list(na.rm=T)))
            
            ## Sep   244 + 30 = 274
            out[,,9] <- matrix(mapply(sum, 
                                      daily.tmp[,,245],daily.tmp[,,246],
                                      daily.tmp[,,247],daily.tmp[,,248],daily.tmp[,,249],daily.tmp[,,250],
                                      daily.tmp[,,251],daily.tmp[,,252],daily.tmp[,,253],
                                      daily.tmp[,,254],daily.tmp[,,255],daily.tmp[,,256],
                                      daily.tmp[,,257],daily.tmp[,,258],daily.tmp[,,259],daily.tmp[,,260],
                                      daily.tmp[,,261],daily.tmp[,,262],daily.tmp[,,263],
                                      daily.tmp[,,264],daily.tmp[,,265],daily.tmp[,,266],
                                      daily.tmp[,,267],daily.tmp[,,268],daily.tmp[,,269],daily.tmp[,,270],
                                      daily.tmp[,,271],daily.tmp[,,272],daily.tmp[,,273],daily.tmp[,,274],
                                      MoreArgs=list(na.rm=T)))
            
            ## Oct   274 + 31 = 305
            out[,,10] <- matrix(mapply(sum, 
                                      daily.tmp[,,275],daily.tmp[,,276],
                                      daily.tmp[,,277],daily.tmp[,,278],daily.tmp[,,279],daily.tmp[,,280],
                                      daily.tmp[,,281],daily.tmp[,,282],daily.tmp[,,283],
                                      daily.tmp[,,284],daily.tmp[,,285],daily.tmp[,,286],
                                      daily.tmp[,,287],daily.tmp[,,288],daily.tmp[,,289],daily.tmp[,,290],
                                      daily.tmp[,,291],daily.tmp[,,292],daily.tmp[,,293],
                                      daily.tmp[,,294],daily.tmp[,,295],daily.tmp[,,296],
                                      daily.tmp[,,297],daily.tmp[,,298],daily.tmp[,,299],daily.tmp[,,300],
                                      daily.tmp[,,301],daily.tmp[,,302],daily.tmp[,,303],
                                      daily.tmp[,,304],daily.tmp[,,305],
                                      MoreArgs=list(na.rm=T)))
            
            ## Nov   305 + 30 = 335
            out[,,11] <- matrix(mapply(sum, 
                                       daily.tmp[,,306],
                                       daily.tmp[,,307],daily.tmp[,,308],daily.tmp[,,309],daily.tmp[,,310],
                                       daily.tmp[,,311],daily.tmp[,,312],daily.tmp[,,313],
                                       daily.tmp[,,314],daily.tmp[,,315],daily.tmp[,,316],
                                       daily.tmp[,,317],daily.tmp[,,318],daily.tmp[,,319],daily.tmp[,,320],
                                       daily.tmp[,,321],daily.tmp[,,322],daily.tmp[,,323],
                                       daily.tmp[,,324],daily.tmp[,,325],daily.tmp[,,326],
                                       daily.tmp[,,327],daily.tmp[,,328],daily.tmp[,,329],daily.tmp[,,330],
                                       daily.tmp[,,331],daily.tmp[,,332],daily.tmp[,,333],
                                       daily.tmp[,,334],daily.tmp[,,335],
                                       MoreArgs=list(na.rm=T)))
            
            ## Dec   335 + 31 = 366
            out[,,12] <- matrix(mapply(sum, 
                                       daily.tmp[,,336],
                                       daily.tmp[,,337],daily.tmp[,,338],daily.tmp[,,339],daily.tmp[,,340],
                                       daily.tmp[,,341],daily.tmp[,,342],daily.tmp[,,343],
                                       daily.tmp[,,344],daily.tmp[,,345],daily.tmp[,,346],
                                       daily.tmp[,,347],daily.tmp[,,348],daily.tmp[,,349],daily.tmp[,,350],
                                       daily.tmp[,,351],daily.tmp[,,352],daily.tmp[,,353],
                                       daily.tmp[,,354],daily.tmp[,,355],daily.tmp[,,356],
                                       daily.tmp[,,357],daily.tmp[,,358],daily.tmp[,,359],daily.tmp[,,360],
                                       daily.tmp[,,361],daily.tmp[,,362],daily.tmp[,,363],
                                       daily.tmp[,,364],daily.tmp[,,365],daily.tmp[,,366],
                                       MoreArgs=list(na.rm=T)))
            
        } else {
            ## Feb   31 + 28 = 59
            out[,,2] <- matrix(mapply(sum, daily.tmp[,,32],daily.tmp[,,33],
                                      daily.tmp[,,34],daily.tmp[,,35],daily.tmp[,,36],
                                      daily.tmp[,,37],daily.tmp[,,38],daily.tmp[,,39],daily.tmp[,,40],
                                      daily.tmp[,,41],daily.tmp[,,42],daily.tmp[,,43],
                                      daily.tmp[,,44],daily.tmp[,,45],daily.tmp[,,46],
                                      daily.tmp[,,47],daily.tmp[,,48],daily.tmp[,,49],daily.tmp[,,50],
                                      daily.tmp[,,51],daily.tmp[,,52],daily.tmp[,,53],
                                      daily.tmp[,,54],daily.tmp[,,55],daily.tmp[,,56],
                                      daily.tmp[,,57],daily.tmp[,,58],daily.tmp[,,59],
                                      MoreArgs=list(na.rm=T)))
            
            ## Mar    59 + 31 = 90
            out[,,3] <- matrix(mapply(sum,daily.tmp[,,60],
                                      daily.tmp[,,61],daily.tmp[,,62],daily.tmp[,,63],
                                      daily.tmp[,,64],daily.tmp[,,65],daily.tmp[,,66],
                                      daily.tmp[,,67],daily.tmp[,,68],daily.tmp[,,69],daily.tmp[,,70],
                                      daily.tmp[,,71],daily.tmp[,,72],daily.tmp[,,73],
                                      daily.tmp[,,74],daily.tmp[,,75],daily.tmp[,,76],
                                      daily.tmp[,,77],daily.tmp[,,78],daily.tmp[,,79],daily.tmp[,,80],
                                      daily.tmp[,,81],daily.tmp[,,82],daily.tmp[,,83],
                                      daily.tmp[,,84],daily.tmp[,,85],daily.tmp[,,86],
                                      daily.tmp[,,87],daily.tmp[,,88],daily.tmp[,,89],daily.tmp[,,90],
                                      MoreArgs=list(na.rm=T)))
            
            ## Apr    90 + 30 = 120
            out[,,4] <- matrix(mapply(sum, daily.tmp[,,91],daily.tmp[,,92],daily.tmp[,,93],
                                      daily.tmp[,,94],daily.tmp[,,95],daily.tmp[,,96],
                                      daily.tmp[,,97],daily.tmp[,,98],daily.tmp[,,99],daily.tmp[,,100],
                                      daily.tmp[,,101],daily.tmp[,,102],daily.tmp[,,103],
                                      daily.tmp[,,104],daily.tmp[,,105],daily.tmp[,,106],
                                      daily.tmp[,,107],daily.tmp[,,108],daily.tmp[,,109],daily.tmp[,,110],
                                      daily.tmp[,,111],daily.tmp[,,112],daily.tmp[,,113],
                                      daily.tmp[,,114],daily.tmp[,,115],daily.tmp[,,116],
                                      daily.tmp[,,117],daily.tmp[,,118],daily.tmp[,,119],daily.tmp[,,120],
                                      MoreArgs=list(na.rm=T)))
            
            ## May    120 + 31 = 151
            out[,,5] <- matrix(mapply(sum, daily.tmp[,,121],daily.tmp[,,122],daily.tmp[,,123],
                                      daily.tmp[,,124],daily.tmp[,,125],daily.tmp[,,126],
                                      daily.tmp[,,127],daily.tmp[,,128],daily.tmp[,,129],daily.tmp[,,130],
                                      daily.tmp[,,131],daily.tmp[,,132],daily.tmp[,,133],
                                      daily.tmp[,,134],daily.tmp[,,135],daily.tmp[,,136],
                                      daily.tmp[,,137],daily.tmp[,,138],daily.tmp[,,139],daily.tmp[,,140],
                                      daily.tmp[,,141],daily.tmp[,,142],daily.tmp[,,143],
                                      daily.tmp[,,144],daily.tmp[,,145],daily.tmp[,,146],
                                      daily.tmp[,,147],daily.tmp[,,148],daily.tmp[,,149],daily.tmp[,,150],
                                      daily.tmp[,,151],
                                      MoreArgs=list(na.rm=T)))
            
            ## Jun    151 + 30 = 181
            out[,,6] <- matrix(mapply(sum, daily.tmp[,,152],daily.tmp[,,153],
                                      daily.tmp[,,154],daily.tmp[,,155],daily.tmp[,,156],
                                      daily.tmp[,,157],daily.tmp[,,158],daily.tmp[,,159],daily.tmp[,,160],
                                      daily.tmp[,,161],daily.tmp[,,162],daily.tmp[,,163],
                                      daily.tmp[,,164],daily.tmp[,,165],daily.tmp[,,166],
                                      daily.tmp[,,167],daily.tmp[,,168],daily.tmp[,,169],daily.tmp[,,170],
                                      daily.tmp[,,171],daily.tmp[,,172],daily.tmp[,,173],
                                      daily.tmp[,,174],daily.tmp[,,175],daily.tmp[,,176],
                                      daily.tmp[,,177],daily.tmp[,,178],daily.tmp[,,179],daily.tmp[,,180],
                                      daily.tmp[,,181],
                                      MoreArgs=list(na.rm=T)))
            
            ## Jul    181 + 31 = 212
            out[,,7] <- matrix(mapply(sum, daily.tmp[,,182],daily.tmp[,,183],
                                      daily.tmp[,,184],daily.tmp[,,185],daily.tmp[,,186],
                                      daily.tmp[,,187],daily.tmp[,,188],daily.tmp[,,189],daily.tmp[,,190],
                                      daily.tmp[,,191],daily.tmp[,,192],daily.tmp[,,193],
                                      daily.tmp[,,194],daily.tmp[,,195],daily.tmp[,,196],
                                      daily.tmp[,,197],daily.tmp[,,198],daily.tmp[,,199],daily.tmp[,,200],
                                      daily.tmp[,,201],daily.tmp[,,202],daily.tmp[,,203],
                                      daily.tmp[,,204],daily.tmp[,,205],daily.tmp[,,206],
                                      daily.tmp[,,207],daily.tmp[,,208],daily.tmp[,,209],daily.tmp[,,210],
                                      daily.tmp[,,211],daily.tmp[,,212],
                                      MoreArgs=list(na.rm=T)))
            
            ## Aug    212 + 31 = 243
            out[,,8] <- matrix(mapply(sum, daily.tmp[,,213],
                                      daily.tmp[,,214],daily.tmp[,,215],daily.tmp[,,216],
                                      daily.tmp[,,217],daily.tmp[,,218],daily.tmp[,,219],daily.tmp[,,220],
                                      daily.tmp[,,221],daily.tmp[,,222],daily.tmp[,,223],
                                      daily.tmp[,,224],daily.tmp[,,225],daily.tmp[,,226],
                                      daily.tmp[,,227],daily.tmp[,,228],daily.tmp[,,229],daily.tmp[,,230],
                                      daily.tmp[,,231],daily.tmp[,,232],daily.tmp[,,233],
                                      daily.tmp[,,234],daily.tmp[,,235],daily.tmp[,,236],
                                      daily.tmp[,,237],daily.tmp[,,238],daily.tmp[,,239],daily.tmp[,,240],
                                      daily.tmp[,,241],daily.tmp[,,242],daily.tmp[,,243],
                                      MoreArgs=list(na.rm=T)))
            
            ## Sep   243 + 30 = 273
            out[,,9] <- matrix(mapply(sum, 
                                      daily.tmp[,,244],daily.tmp[,,245],daily.tmp[,,246],
                                      daily.tmp[,,247],daily.tmp[,,248],daily.tmp[,,249],daily.tmp[,,250],
                                      daily.tmp[,,251],daily.tmp[,,252],daily.tmp[,,253],
                                      daily.tmp[,,254],daily.tmp[,,255],daily.tmp[,,256],
                                      daily.tmp[,,257],daily.tmp[,,258],daily.tmp[,,259],daily.tmp[,,260],
                                      daily.tmp[,,261],daily.tmp[,,262],daily.tmp[,,263],
                                      daily.tmp[,,264],daily.tmp[,,265],daily.tmp[,,266],
                                      daily.tmp[,,267],daily.tmp[,,268],daily.tmp[,,269],daily.tmp[,,270],
                                      daily.tmp[,,271],daily.tmp[,,272],daily.tmp[,,273],
                                      MoreArgs=list(na.rm=T)))
            
            ## Oct   273 + 31 = 304
            out[,,10] <- matrix(mapply(sum, 
                                       daily.tmp[,,274],daily.tmp[,,275],daily.tmp[,,276],
                                       daily.tmp[,,277],daily.tmp[,,278],daily.tmp[,,279],daily.tmp[,,280],
                                       daily.tmp[,,281],daily.tmp[,,282],daily.tmp[,,283],
                                       daily.tmp[,,284],daily.tmp[,,285],daily.tmp[,,286],
                                       daily.tmp[,,287],daily.tmp[,,288],daily.tmp[,,289],daily.tmp[,,290],
                                       daily.tmp[,,291],daily.tmp[,,292],daily.tmp[,,293],
                                       daily.tmp[,,294],daily.tmp[,,295],daily.tmp[,,296],
                                       daily.tmp[,,297],daily.tmp[,,298],daily.tmp[,,299],daily.tmp[,,300],
                                       daily.tmp[,,301],daily.tmp[,,302],daily.tmp[,,303],
                                       daily.tmp[,,304],
                                       MoreArgs=list(na.rm=T)))
            
            ## Nov   304 + 30 = 334
            out[,,11] <- matrix(mapply(sum, 
                                       daily.tmp[,,305],daily.tmp[,,306],
                                       daily.tmp[,,307],daily.tmp[,,308],daily.tmp[,,309],daily.tmp[,,310],
                                       daily.tmp[,,311],daily.tmp[,,312],daily.tmp[,,313],
                                       daily.tmp[,,314],daily.tmp[,,315],daily.tmp[,,316],
                                       daily.tmp[,,317],daily.tmp[,,318],daily.tmp[,,319],daily.tmp[,,320],
                                       daily.tmp[,,321],daily.tmp[,,322],daily.tmp[,,323],
                                       daily.tmp[,,324],daily.tmp[,,325],daily.tmp[,,326],
                                       daily.tmp[,,327],daily.tmp[,,328],daily.tmp[,,329],daily.tmp[,,330],
                                       daily.tmp[,,331],daily.tmp[,,332],daily.tmp[,,333],
                                       daily.tmp[,,334],
                                       MoreArgs=list(na.rm=T)))
            
            ## Dec   334 + 31 = 365
            out[,,12] <- matrix(mapply(sum, 
                                       daily.tmp[,,335],daily.tmp[,,336],
                                       daily.tmp[,,337],daily.tmp[,,338],daily.tmp[,,339],daily.tmp[,,340],
                                       daily.tmp[,,341],daily.tmp[,,342],daily.tmp[,,343],
                                       daily.tmp[,,344],daily.tmp[,,345],daily.tmp[,,346],
                                       daily.tmp[,,347],daily.tmp[,,348],daily.tmp[,,349],daily.tmp[,,350],
                                       daily.tmp[,,351],daily.tmp[,,352],daily.tmp[,,353],
                                       daily.tmp[,,354],daily.tmp[,,355],daily.tmp[,,356],
                                       daily.tmp[,,357],daily.tmp[,,358],daily.tmp[,,359],daily.tmp[,,360],
                                       daily.tmp[,,361],daily.tmp[,,362],daily.tmp[,,363],
                                       daily.tmp[,,364],daily.tmp[,,365],
                                       MoreArgs=list(na.rm=T)))
        }  # end of if statement
        
        saveRDS(out, file=paste0(destDir, "/DF", i, ".rds"))
        
        
    }  # i loop
}   # function loop