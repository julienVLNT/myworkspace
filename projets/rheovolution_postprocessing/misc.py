#! -*- coding: utf-8 -*-

__author__ = "Julien VALENTIN"
__email__  = "julien.valentin@umontpellier.fr"

import image             as img
import matplotlib.pyplot as plt
import matplotlib.tri    as tri
import numpy             as np
import os
import scipy             as sp


def autocorrelation_analysis(data: np.ndarray, name: str, it: int, outfolder: str, folder: str) -> None:
    """Analysis based on autocorrelation. Computes autocorrelation, C_infinity, and other features,
    then export visual reports and a .csv dataset.

    Params
    ------
        data      : np.ndarray ; scalar field
        name      : str        ; name for graphics titling
        it        : int        ; index of the date
        outfolder : str        ; root folder to save the picture (consistent with )
        folder    : str        ; complete path to the folder of the simulation

    Return
    ------
        None.
    """
    data = np.nan_to_num(data, nan=np.nanmean(data))
    N    = data.shape[0]

    nrows = 5
    ncols = 3
    fig   = plt.figure(figsize=(ncols*15, nrows*15))

    axe = fig.add_subplot(nrows,ncols,1)
    axe.set_title(name, fontsize=30)
    axe.set_xlabel("X [px]", fontsize=20)
    axe.set_ylabel("Z [px]", fontsize=20)
    art = axe.imshow(data,origin="lower")
    plt.colorbar(art)

    auto = img.autocorrelation(data)
    cinf = img.cinfinity(data)
    axe = fig.add_subplot(nrows,ncols, 2)
    axe.set_title(r"$\mathcal{A}($"+name+r")", fontsize=30)
    axe.set_xlabel("X [px]", fontsize=20)
    axe.set_ylabel("Z [px]", fontsize=20)
    art = axe.imshow(auto, origin="lower")
    axe.contour(auto, [cinf, 1.0])
    plt.colorbar(art)

    hor, ver = img.profiles(auto)
    axe = fig.add_subplot(nrows, ncols, 3)
    axe.set_title("Profiles", fontsize=30)
    axe.set_xlabel("radius [px]", fontsize=20)
    axe.set_ylabel(r"$\mathcal{A}($"+name+r")", fontsize=20)
    axe.plot(hor, color='red',   label=r"$\theta=0°$",  marker='o')
    axe.plot(ver, color='green', label=r"$\theta=90°$", marker='o')
    axe.hlines(y=cinf, xmin=0, xmax=auto.shape[0])
    axe.legend()

    angles = np.linspace(0, 179, 180)
    radius = np.zeros_like(angles) * np.nan
    for k in range(len(angles)):
        radius[k] = img.length_vs_angle(auto, cinf, angles[k]*np.pi/180)
    axe = fig.add_subplot(nrows, ncols, 4)
    axe.set_title(r"Radius to $C_\infty$", fontsize=30)
    axe.set_xlabel(r"$\theta$ [°]", fontsize=20)
    axe.set_ylabel(r"$R(\theta)$ [px]", fontsize=20)
    axe.plot(angles, radius, marker='o')

    angle = angles[radius.argmax()]*np.pi/180 + np.pi/2
    r, a  = img.radial_profile(auto, angle)
    r = np.concatenate((-np.flip(r), r))
    a = np.concatenate((np.flip(a), a))
    peaks, properties = sp.signal.find_peaks(a, distance=1, height=cinf, plateau_size=0, prominence=0, rel_height=1, threshold=0, width=1, wlen=None)
    print()
    print(f"Peaks and properties : t {str(it).zfill(3)}")
    print(f"----------------------------")
    print("Peaks  :", peaks)
    print("Radius :", r[peaks])
    print("Autocor:", a[peaks])
    for key, val in properties.items():
        print(key)
        print("\t", val)
    axe = fig.add_subplot(nrows, ncols, 5)
    axe.set_title(r"Profil : $\theta = $"+"{:.2f}".format(angle*180/np.pi)+"°", fontsize=30)
    axe.set_xlabel(r"radius [px]", fontsize=20)
    axe.set_ylabel(r"$\mathcal{A}($"+name+r") [px]", fontsize=20)
    axe.plot(r, a, marker='o')
    axe.hlines(y=cinf, xmin=r.min(), xmax=r.max(), color="C1")
    axe.plot(r[peaks], a[peaks], "x", color="C2")
    axe.vlines(x=r[peaks], ymin=a.min(), ymax=a[peaks], color="C2")
    axe.vlines(x=r[peaks], ymin=a.min(), ymax=a[peaks], color="C3")

    # angles_ = np.linspace(0, 2*np.pi, 360)
    # radius_ = np.hstack((radius, radius))
    # axe = fig.add_subplot(nrows, ncols, 6, projection="polar")
    # axe.fill(angles_, radius_, color="tan")
    # axe.set_rmax(radius.max())

    power = np.abs(np.fft.fft(a))
    freqs = np.fft.fftfreq(N, d=1)
    axe = fig.add_subplot(nrows, ncols, 7)
    axe.semilogy(freqs, power, '--x')
    axe.set_title(r"Power Spectrum", fontsize=30)
    axe.set_xlabel(r"Frequencies [Hz]", fontsize=20)
    axe.set_ylabel(r"$\mathcal{F}\mathcal{A}$", fontsize=20)

    power  = np.abs(np.fft.fft2(auto))
    power  = np.fft.fftshift(power)
    freqsx = np.fft.fftshift(np.fft.fftfreq(N, d=1))
    freqsy = np.fft.fftshift(np.fft.fftfreq(N, d=1))
    axe    = fig.add_subplot(nrows,ncols,8)
    g = axe.imshow(power)
    plt.colorbar(g)
    axe.set_title(r"$\mathcal{F}\mathcal{A}$"+name, fontsize=30)
    axe.set_xlabel("X [Hz]", fontsize=20)
    axe.set_xticks(ticks=list(range(0,256,16)), labels=freqsx[::16])
    axe.set_ylabel("Z [Hz]", fontsize=20)
    axe.set_yticks(ticks=list(range(0,256,8)), labels=freqsy[::8])

    # if it > 1:
    #     maxima = local_maxima(np.log(power))
    #     maxima = np.sort(maxima, axis=1)
    #     axe = fig.add_subplot(nrows,ncols,9)
    #     g = axe.imshow(np.log(power))
    #     for i in range(maxima.shape[1]):
    #         axe.scatter(maxima[0,i],maxima[1,i],color=f"C{i}",label=f"{i}")
    #     axe.legend()
    #     plt.colorbar(g)
    #     axe.set_title(r"$\log(\mathcal{F}\mathcal{A}$"+name+")", fontsize=30)
    #     axe.set_xlabel("X [Hz]", fontsize=20)
    #     axe.set_xticks(ticks=list(range(0,256,16)), labels=freqsx[::16])
    #     axe.set_ylabel("Z [Hz]", fontsize=20)
    #     axe.set_yticks(ticks=list(range(0,256,8)), labels=freqsy[::8])

    #     fake = np.zeros_like(power)
    #     fake[maxima[:,0]] = power[maxima[:,0]]
    #     inverse = np.abs(np.fft.fftshift(np.fft.ifft2(fake)))
    #     axe = fig.add_subplot(nrows, ncols, 10)
    #     axe.imshow(np.fliplr(inverse))

    #     fake = np.zeros_like(power)
    #     fake[maxima[:,1]] = power[maxima[:,1]]
    #     inverse = np.abs(np.fft.fftshift(np.fft.ifft2(fake)))
    #     axe = fig.add_subplot(nrows, ncols, 11)
    #     axe.imshow(np.fliplr(inverse))

    #     fake = np.zeros_like(power)
    #     fake[maxima[:,2]] = power[maxima[:,2]]
    #     inverse = np.abs(np.fft.fftshift(np.fft.ifft2(fake)))
    #     axe = fig.add_subplot(nrows, ncols, 12)
    #     axe.imshow(np.fliplr(inverse))

    #     fake = np.zeros_like(power)
    #     fake[maxima[:,3]] = power[maxima[:,3]]
    #     inverse = np.abs(np.fft.fftshift(np.fft.ifft2(fake)))
    #     axe = fig.add_subplot(nrows, ncols, 13)
    #     axe.imshow(np.fliplr(inverse))

    #     fake = np.zeros_like(power)
    #     fake[maxima[:,4]] = power[maxima[:,4]]
    #     inverse = np.abs(np.fft.fftshift(np.fft.ifft2(fake)))
    #     axe = fig.add_subplot(nrows, ncols, 14)
    #     axe.imshow(np.fliplr(inverse))

    angles_ = np.linspace(0, 2*np.pi, 360)
    radius_ = np.hstack((radius, radius))
    axe = fig.add_subplot(nrows, ncols, 6, projection="polar")
    axe.fill(angles_, radius_, color="tan")
    axe.set_rmax(radius.max())

    # if it > 0:
    #     fft_res = np.fft.fft2(np.fliplr(data))
    #     var_tmp = np.abs(fft_res)
    #     var_tmp[-3:3,-3:3] = 0
    #     freqs   = np.fft.fftfreq(N,d=1)
    #     coords  = local_maxima(data)
    #     fakefft = np.zeros_like(fft_res)
    #     fakefft[coords[0,:],coords[1,:]] = fft_res[coords[0,:],coords[1,:]]
    #     plop = np.fft.ifft2(fakefft)
    #     axe = fig.add_subplot(nrows,ncols,10)
    #     g = axe.imshow(np.abs(plop))
    #     axe.scatter(coords[0,:], coords[1,:], color="C1")
    #     plt.colorbar(g)

    # axe = fig.add_subplot(nrows,ncols,10)
    # image_max = sp.ndimage.filters.maximum_filter(power, size=20, mode='constant')
    # g = axe.imshow(image_max)
    # plt.colorbar(g)
    # axe.set_title(r"Max filter", fontsize=30)
    # axe.set_xlabel("X [Hz]", fontsize=20)
    # axe.set_xticks(ticks=list(range(0,256,16)), labels=freqsx[::16])
    # axe.set_ylabel("Z [Hz]", fontsize=20)
    # axe.set_yticks(ticks=list(range(0,256,8)), labels=freqsy[::8])

    # power  = np.fft.fftshift(np.abs(np.fft.fft2(data)))
    # freqsx = np.fft.fftshift(np.fft.fftfreq(N, d=1))
    # freqsy = np.fft.fftshift(np.fft.fftfreq(N, d=1))
    # axe    = fig.add_subplot(nrows,ncols,9)
    # g      = axe.imshow(np.log(power))
    # levels = [0.0, 0.9*np.nanmax(power), np.nanmax(power)]
    # if np.nanmax(power) > 0:
    #     axe.contour(np.log(power), levels)
    # plt.colorbar(g)
    # axe.set_title(r"$\log\mathcal{F}$"+name+", shifted, contour = 90%", fontsize=30)
    # axe.set_xlabel("X [Hz]", fontsize=20)
    # axe.set_xticks(ticks=list(range(0,256,16)), labels=freqsx[::16])
    # axe.set_ylabel("Z [Hz]", fontsize=20)
    # axe.set_yticks(ticks=list(range(0,256,8)), labels=freqsy[::8])

    # maximum = np.where(power>0.0001*np.nanmax(power), power, 0)
    # axe     = fig.add_subplot(nrows, ncols, 10)
    # axe.imshow(np.log(maximum), cmap='gray')
    # axe.set_title("Maxima in the 2D spectrum", fontsize=30)
    # axe.set_xlabel("X [Hz]", fontsize=20)
    # axe.set_xticks(ticks=list(range(0,256,16)), labels=freqsx[::16])
    # axe.set_ylabel("Z [Hz]", fontsize=20)
    # axe.set_yticks(ticks=list(range(0,256,8)), labels=freqsy[::8])

    # spectrum = np.fft.fftshift(np.fft.fft2(maximum))
    # axe      = fig.add_subplot(nrows, ncols, 11)
    # axe.imshow(np.log(np.abs(spectrum)), cmap='gray')

    # rebuilt = np.fft.ifft2(np.fft.ifftshift(maximum))
    # axe      = fig.add_subplot(nrows, ncols, 12)
    # axe.imshow(np.log(np.abs(rebuilt)))

    plt.savefig(f"{outfolder}/analysis/singles/{os.path.basename(folder)}/analysis_t="+str(it).zfill(3)+".jpg")
    plt.close()

    return None


def compute_j2_invariant(data: np.ndarray) -> np.ndarray:
    """Computes the J2-invariant for a numpy ndarray with six components in the third dimension.

    Params
    ------
        data : np.ndarray, dim=3, shape=(...,6) ; field values distributed as scalar components.

    Returns
    -------
        j2 : np.ndarray, dim = 2, shape=(...) ; j2-invariant of the tensor.
    """
    p  = data[:,:,:3].sum(axis=2) /3.0
    j2 = np.sqrt( 3./2 *np.power( data[:,:,:3] -1./3*p[:,:,np.newaxis], 2).sum(axis=2) \
                                + 3. *np.power(data[:,:,3:], 2).sum(axis=2)            )

    return j2


def describe(ndarray: np.ndarray) -> None:
    """Provides basic statistics of an instance of numpy.ndarray.
    """
    print(f"dim    : {ndarray.ndim}")
    print(f"shape  : {ndarray.shape}")
    print(f"dtype  : {ndarray.dtype}")

    if ndarray.ndim > 1 : ndarray = ndarray.flatten()

    print(f"NaNs   : {np.any(np.isnan(ndarray))}")
    print(f"min    : {np.nanmin(ndarray)}")
    print(f"max    : {np.nanmax(ndarray)}")
    print(f"mean   : {np.nanmean(ndarray)}")
    print(f"median : {np.nanmedian(ndarray)}")
    print(f"std    : {np.nanstd(ndarray)}")
    print(f"skew   : {sp.stats.skew(ndarray, bias=True, nan_policy='omit')}")
    print(f"kurt.  : {sp.stats.kurtosis(ndarray, bias=True, fisher=True, nan_policy='omit')}")

    return None


def devoigt_to_symmetric_matrix(data: np.ndarray) -> np.ndarray:
    """Converts a symmetric tensor from its de Voigt representation into its symmetric matrix representation.
    The data should be presented with diagonal terms first and then the upper part of the matrix, starting 
    with the first line...

    Params
    ------
        data : np.ndarray, dim=1 ; De Voigt vector

    Returns
    -------
        matrix : np.ndarray, dim=2 ; Symmetric matrix
    """

    def to_2x2_matrix(data: np.ndarray) -> np.ndarray:
        """Special case for a 3-length vector. 
        """
        M = np.zeros((2,2), dtype=np.float64)
        M[0,0] = data[0]
        M[0,1] = data[2]
        M[1,0] = data[2]
        M[1,1] = data[1]
        return M
    
    def to_3x3_matrix(data: np.ndarray) -> np.ndarray:
        """Special case for a 6-length vector.
        """
        M = np.zeros((3,3), dtype=np.float64)
        M[0,0] = data[0]
        M[0,1] = data[3]
        M[0,2] = data[4]
        M[1,0] = data[3]
        M[1,1] = data[1]
        M[1,2] = data[5]
        M[2,0] = data[4]
        M[2,1] = data[5]
        M[2,2] = data[2]
        return M
    
    def to_6x6_matrix(data: np.ndarray) -> np.ndarray:
        """Special case for a 21-length vector.
        """
        M = np.zeros((6,6), dtype=np.float64)
        M[0,0] = data[0]
        M[0,1] = data[6]
        M[0,2] = data[7]
        M[0,3] = data[8]
        M[0,4] = data[9]
        M[0,5] = data[10]
        M[1,0] = data[6]
        M[1,1] = data[1]
        M[1,2] = data[11]
        M[1,3] = data[12]
        M[1,4] = data[13]
        M[1,5] = data[14]
        M[2,0] = data[7]
        M[2,1] = data[11]
        M[2,2] = data[2]
        M[2,3] = data[15]
        M[2,4] = data[16]
        M[2,5] = data[17]
        M[3,0] = data[8]
        M[3,1] = data[12]
        M[3,2] = data[15]
        M[3,3] = data[3]
        M[3,4] = data[18]
        M[3,5] = data[19]
        M[4,0] = data[9]
        M[4,1] = data[13]
        M[4,2] = data[16]
        M[4,3] = data[18]
        M[4,4] = data[4]
        M[4,5] = data[20]
        M[5,0] = data[10]
        M[5,1] = data[14]
        M[5,2] = data[17]
        M[5,3] = data[19]
        M[5,4] = data[20]
        M[5,5] = data[5]
        return M
    
    if   data.shape[0] ==  3: M = to_2x2_matrix(data)
    elif data.shape[0] ==  6: M = to_3x3_matrix(data)
    elif data.shape[0] == 21: M = to_6x6_matrix(data)
    else: M = None

    return M


def devoigt_to_antisymmetric_matrix(data: np.ndarray) -> np.ndarray:
    """Converts a symmetric tensor from its de Voigt representation into its antisymmetric matrix representation.
    The data should be presented with diagonal terms first and then the upper part of the matrix, starting 
    with the first line... 

    Params
    ------
        data : np.ndarray, dim=1 ; De Voigt vector

    Returns
    -------
        matrix : np.ndarray, dim=2 ; antisymmetric matrix
    """

    def to_2x2_matrix(data: np.ndarray) -> np.ndarray:
        """Special case for a 3-length vector. 
        """
        M = np.zeros((2,2), dtype=np.float64)
        M[0,0] =  data[0]
        M[0,1] =  data[2]
        M[1,0] = -data[2]
        M[1,1] =  data[1]
        return M
    
    def to_3x3_matrix(data: np.ndarray) -> np.ndarray:
        """Special case for a 6-length vector.
        """
        M = np.zeros((3,3), dtype=np.float64)
        M[0,0] =  data[0]
        M[0,1] =  data[3]
        M[0,2] =  data[4]
        M[1,0] = -data[3]
        M[1,1] =  data[1]
        M[1,2] =  data[5]
        M[2,0] = -data[4]
        M[2,1] = -data[5]
        M[2,2] =  data[2]
        return M
    
    def to_6x6_matrix(data: np.ndarray) -> np.ndarray:
        """Special case for a 21-length vector.
        """
        M = np.zeros((6,6), dtype=np.float64)
        M[0,0] =  data[0]
        M[0,1] =  data[6]
        M[0,2] =  data[7]
        M[0,3] =  data[8]
        M[0,4] =  data[9]
        M[0,5] =  data[10]
        M[1,0] = -data[6]
        M[1,1] =  data[1]
        M[1,2] =  data[11]
        M[1,3] =  data[12]
        M[1,4] =  data[13]
        M[1,5] =  data[14]
        M[2,0] = -data[7]
        M[2,1] = -data[11]
        M[2,2] =  data[2]
        M[2,3] =  data[15]
        M[2,4] =  data[16]
        M[2,5] =  data[17]
        M[3,0] = -data[8]
        M[3,1] = -data[12]
        M[3,2] = -data[15]
        M[3,3] =  data[3]
        M[3,4] =  data[18]
        M[3,5] =  data[19]
        M[4,0] = -data[9]
        M[4,1] = -data[13]
        M[4,2] = -data[16]
        M[4,3] = -data[18]
        M[4,4] =  data[4]
        M[4,5] =  data[20]
        M[5,0] = -data[10]
        M[5,1] = -data[14]
        M[5,2] = -data[17]
        M[5,3] = -data[19]
        M[5,4] = -data[20]
        M[5,5] =  data[5]
        return M
    
    if   data.shape[0] ==  3: M = to_2x2_matrix(data)
    elif data.shape[0] ==  6: M = to_3x3_matrix(data)
    elif data.shape[0] == 21: M = to_6x6_matrix(data)
    else: M = None

    return M


def export_field(data: np.ndarray, mesh: tri.Triangulation, it: int, name: str, outfolder: str, folder: str) -> None:
    """Export the picture of a scalar field drawn onto a 2D-mesh.

    Params
    ------
        data      : np.ndarray, dim=1 ; the values of the field
        mesh      : tri.Triangulation ; the 2D-mesh of the face
        it        : int               ; the index of the time, to build the file name
        name      : str               ; the name of the field, to build the file name
        outfolder : str               ; to build the path to the saved picture, consistent with prepare_tree()
        folder    : str               ; the name of the simulation, to build the path to the saved picture, consistent with prepare_tree()

    Returns
    -------
        None
    """
    fig = plt.figure(figsize=(30,30))
    axe = fig.add_subplot(111)
    art = axe.tricontourf(mesh, data)
    axe.set_title(f"{name} ; time step : {it+1}", fontsize=40)
    axe.set_xlabel(f"X coord [m]", fontsize=30)
    axe.set_ylabel(f"Z coord [m]", fontsize=30)
    plt.colorbar(art)
    plt.savefig(f"{outfolder}/analysis/singles/{os.path.basename(folder)}/{name}/{name}_t="+str(it+1).zfill(3)+".jpg")
    plt.close()

    return None


def export_mesh(mesh: tri.Triangulation, xlin: np.ndarray, zlin: np.ndarray, it: int, outfolder: str, folder: str) -> None:
    """Builds then saves the plot of an unstructured 2D mesh.
    
    Params
    ------
    mesh      : tri.Triangulation ; the mesh of the face.
    xlin      : np.ndarray, dim=2 ; the list of x-coords, indexed by (row, col).
    zlin      : np.ndarray, dim=2 ; the list of z-coords, indexed by (row, col)
    it        : int               ; the index of the date
    outfolder : str               ;

    Returns
    -------
        None
    """
    fig = plt.figure(figsize=(30,30))
    axe = fig.add_subplot(111)
    axe.triplot(mesh)
    axe.scatter(xlin, zlin, color='red', s=1)
    axe.set_title(f"time step : {it+1}", fontsize=40)
    axe.set_xlabel(f"X coord [m]", fontsize=30)
    axe.set_ylabel(f"Z coord [m]", fontsize=30)
    plt.savefig(f"{outfolder}/analysis/singles/{os.path.basename(folder)}/Mesh/mesh_t="+str(it+1).zfill(3)+".jpg")
    plt.close()

    return None


def inspect(obj: object) -> None:
    """Inspects the properties of a custom objet.
    
    Params
    ------
        obj : Python object.
    
    Returns
    -------
        None
    """
    for key, val in obj.__dict__.items():
        if not key.startswith("__"):
            print(f"{str(key).rjust(12)} : {str(val)}")

    return None


def local_maxima(data: np.ndarray) -> tuple:
    """Hello, World !
    """
    threshold    = 1e-4
    neighborhood = 5

    max_mask = sp.ndimage.filters.maximum_filter(data, neighborhood)
    min_mask = sp.ndimage.filters.minimum_filter(data, neighborhood)
    dif_mask = (np.abs((max_mask - min_mask)) > threshold)
    
    maxima   = (data > 0.1*np.nanmax(data))
    maxima[dif_mask==0] = 0

    labeled, num_objects = sp.ndimage.label(maxima)
    slices = sp.ndimage.find_objects(labeled)
    x, y = [], []
    for dy, dx in slices:
        x_center = (dx.start + dx.stop - 1)/2
        x.append(x_center)
        y_center = (dy.start + dy.stop - 1)/2    
        y.append(y_center)

    fb = np.asarray([x,y], dtype=np.int32)
    return fb


def statistics(data: np.ndarray) -> np.ndarray:
    """Builds the table of statistics for a 2-dimensional array `data` with data[i,j] is the value at time ti, node pj.

    Params
    ------
        data : np.ndarray

    Returns
    -------
        statistics : np.ndarray
    """
    statistics = np.zeros((7, data.shape[0])) * np.nan

    statistics[0,:] = np.nanmin(data, axis=1)
    statistics[1,:] = np.nanmax(data, axis=1)
    statistics[2,:] = np.nanmean(data, axis=1)
    statistics[3,:] = np.nanmedian(data, axis=1)
    statistics[4,:] = np.nanstd(data, axis=1)
    statistics[5,:] = sp.stats.skew(data, axis=1, nan_policy='omit')
    statistics[6,:] = sp.stats.kurtosis(data, axis=1, fisher=True, nan_policy='omit')

    return statistics
