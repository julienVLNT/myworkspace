#!/home/julien/Logiciels/miniconda3/envs/numerics/bin/python
#! -*- coding: utf-8 -*-

__author__ = "Julien VALENTIN"
__email__  = "julien.valentin@umontpellier.fr"

import image
import matplotlib.pyplot as plt
import matplotlib.tri    as tri
import misc
import numpy             as np
import os
import scipy             as sp

class Simulation():

    def __init__(self, path: str) -> None:
        self.path = path
        self.__read_header()
        return None

    def __read_header(self) -> None:

        fields   = []
        topology = []

        try:
            with open(self.path, "r") as stream:

                # Read fields related informations
                number = int(stream.readline().strip())
                for _ in range(number):
                    fields.append(stream.readline().strip())

                # Read topology related informations
                next(stream)
                topology = np.int_(list(filter(None, stream.readline().strip().split(" "))))

                # Update object attributes
                self.nfields = len(fields)
                self.fields  = fields

                self.nelem = topology[0]
                self.nvert = topology[1]
                self.ngaus = topology[2]
                self.ndime = topology[3]
                self.nface = topology[4]
                self.npres = topology[5]

                self.offset = 1 + self.nfields + 3 + self.nelem + 1 + (self.npres//10+1) + 1 + self.nface + 2

        except FileNotFoundError:
            print(f"{self.path} is not readable, skip.")

        return None


    def read_elements(self) -> np.ndarray:
        elements = None

        try:
            elements = np.loadtxt( self.path,
                                   dtype=np.int32,
                                   skiprows=(4 + self.nfields),
                                   max_rows=self.nelem )
            elements = elements[:, 1:]

        except FileNotFoundError:
            print(f"{self.path} is not readable, skip.")

        return elements


    def read_contour(self) -> np.ndarray:
        # contour = None

        # try:
        #     contour = np.genfromtxt( self.path,
        #                               dtype=int,
        #                               skip_header=(4 +self.nfields +self.nelem +1),
        #                               skip_footer=(self.npres//10 +1) )
        #     contour = contour.flatten()

        # except FileNotFoundError:
        #     print(f"{self.path} is not readable, skip.")

        # return contour
        raise NotImplementedError


    def read_faces(self) -> np.ndarray:
        faces = None

        try:
            faces = np.loadtxt( self.path,
                                dtype=np.int32,
                                skiprows=(4 + self.nfields + self.nelem + 1 + int(self.npres//10 + 1) + 1),
                                max_rows=self.nface )
            faces = faces[:, 1:]

        except FileNotFoundError:
            print(f"{self.path} is not readable, skip.")

        return faces


    def read_coords(self, dates: list, names: list) -> np.ndarray:
        coords = np.zeros((len(dates), self.nvert, len(names))) * float('nan')

        coordsmap = { 'x' : 1, 'y' : 2, 'z' : 3,
                      'vx': 4, 'vy': 5, 'vz': 6,
                      'ux': 7, 'uy': 8, 'uz': 9,
                      'T' : 10 }

        try:
            for i, n in enumerate(dates):
                coords[i, :, :] = np.loadtxt( self.path,
                                              dtype=np.float64,
                                              skiprows=int(self.offset + n * (self.nvert+self.nelem+3)),
                                              max_rows=self.nvert,
                                              usecols=[coordsmap[name] for name in names] )

        except FileNotFoundError:
            print(f"{self.path} is not readable, skip.")

        return coords


    def read_fields(self, dates: list, names: list) -> np.ndarray:
        """Reads the tensor of deformations in the p-file.

        Params
        ------
            dates : list(int) ; indices of the out snapshoot.
            names : list(str) ; names as stored in the p-file of the scalar fields.

        Returns
        -------
            fields : np.ndarray, dim=3 ; tensor of deformations [i,j,k] for i-th snap, j-th field and k-th cell
        """
        fields = np.zeros((len(dates), self.nelem, len(names))) * float('nan')

        fieldsmap = dict(
            zip(
                    [name for name in self.fields],
                    [self.fields.index(name)+1 for name in self.fields]
               )
        )

        try:
            for i, n in enumerate(dates):
                if len(names) == 1:
                    var = np.loadtxt( self.path,
                                    dtype=np.float64,
                                    skiprows=int(self.offset + n * (self.nelem+3) + (n+1)*self.nvert),
                                    max_rows=self.nelem,
                                    usecols=[fieldsmap[name] for name in names] )
                    fields[i,:,:] = var[:, np.newaxis]
                else:
                    fields[i,:,:] = np.loadtxt( self.path,
                                    dtype=np.float64,
                                    skiprows=int(self.offset + n * (self.nelem+3) + (n+1)*self.nvert),
                                    max_rows=self.nelem,
                                    usecols=[fieldsmap[name] for name in names] )

        except FileNotFoundError:
            print(f"{self.path} is not readable, skip.")

        return fields


    def read_stress(self, dates: list) -> np.ndarray:
        """Reads the tensor of stress in the p-file.

        Params
        ------
            dates : list(int) ; indices of the out snapshoot.

        Returns
        -------
            deformation : np.ndarray, dim=3 ; tensor of stress [i,j,k] for i-th snap, j-th cell and k-th coordinate of the tensor
        """
        stress = np.zeros((len(dates), self.nelem, 6))

        try:
            for i, n in enumerate(dates):
                stress[i, :, :] = np.loadtxt( self.path,
                                              dtype=np.float64,
                                              skiprows=int(self.offset + n * (self.nelem+3) + (n+1)*self.nvert),
                                              max_rows=self.nelem,
                                              usecols=list(range(1,7)) )

        except FileNotFoundError:
            print(f"{self.path} is not readable, skip.")

        return stress


    def read_strain(self, dates: list) -> np.ndarray:
        """Reads the tensor of strain in the p-file.

        Params
        ------
            dates : list(int) ; indices of the out snapshoot.

        Returns
        -------
            strain : np.ndarray, dim=3 ; tensor of strain [i,j,k] for i-th snap, j-th cell and k-th coordinate of the tensor
        """
        strain = np.zeros((len(dates), self.nelem, 6))

        try:
            for i, n in enumerate(dates):
                strain[i, :, :] = np.loadtxt( self.path,
                                              dtype=np.float64,
                                              skiprows=int(self.offset + n * (self.nelem+3) + (n+1)*self.nvert),
                                              max_rows=self.nelem,
                                              usecols=list(range(7,13)) )

        except FileNotFoundError:
            print(f"{self.path} is not readable, skip.")

        return strain


    def read_deformation(self, dates: list) -> np.ndarray:
        """Reads the tensor of deformations in the p-file.

        Params
        ------
            dates : list(int) ; indices of the out snapshoot.

        Returns
        -------
            deformation : np.ndarray, dim=3 ; tensor of deformations [i,j,k] for i-th snap, j-th cell and k-th coordinate of the tensor
        """
        deformation = np.zeros((len(dates), self.nelem, 6))

        try:
            for i, n in enumerate(dates):
                deformation[i, :, :] = np.loadtxt( self.path,
                                                   dtype=np.float64,
                                                   skiprows=int(self.offset + n * (self.nelem+3) + (n+1)*self.nvert),
                                                   max_rows=self.nelem,
                                                   usecols=list(range(13,19)) )

        except FileNotFoundError:
            print(f"{self.path} is not readable, skip.")

        return deformation


    def compute_invariant(self, dates: list, name: str) -> np.ndarray:
        """Computes the invariant of a tensor represented by its de Voigt vector.

        Params
        ------
            dates : list(int) ; indices of the snapshoots to considere
            name  : str       ; name of the tensor field to read ("D", "E", "S")

        Returns
        -------
            j2 : np.ndarray, dim=2 ; j2-invariant of the tensor, [i,j] for i-th snap and j-th cell
        """
        if   name == "S": field = self.read_stress(dates)
        elif name == "E": field = self.read_strain(dates)
        elif name == "D": field = self.read_deformation(dates)
        else            : raise NotImplementedError

        p  = field[:, :, :3].sum(axis=2) /3.0
        j2 = np.sqrt( 3/2.*np.power(field[:,:,:3]-1/3.*p[:,:,np.newaxis], 2).sum(axis=2) \
                    + 3.*np.power(field[:,:,3:], 2).sum(axis=2)                          )

        return j2


    def compute_work(self, dates: list) -> np.ndarray:
        """Compute work as the pointwise product of strain and deformation.
        
        Params
        ------
            dates : list(int) ; list of dates to read.
        
        Returns
        -------
            W : np.ndarray ; container for work values
        """
        S = self.read_stress(dates)
        E = self.read_strain(dates)
        W = (S*E).sum(axis=2)
        return W


    def compute_work_rate(self, dates: list) -> np.ndarray:
        S  = self.read_stress(dates)
        D  = self.read_deformation(dates)
        WR = (S*D).sum(axis=2)
        return WR


    def nodal_on_face_representation(self, dates: list, name: str) -> np.ndarray:
        """Extracts the data from 3.D cell-wise field to 2.D nodal-wise interpolated values.

        Params
        ------
            dates : list(int) ;
            name  : str       ;

        Returns
        -------
            nodal : np.ndarray ;
        """
        conn = self.read_elements()
        face = self.read_faces()
        ids  = np.where(face[:,0]==1)[0]
        ids  = np.unique(face[ids,1:4].flatten())

        vals = []                              # liste des valeurs du dictionnaire
        for _ in range(len(ids)):              # pour tout indice de noeud sur la facette 1...
            vals.append([])                    # ... créer une liste vide
        rev = dict( zip(ids,vals) )            # créer le dictionnaire qui à chaque indice de noeud de la facette 1 associe la liste vide

        for i, el in enumerate(conn[:,1:]):    # pour tout élément
            for p in el:                       #     pour tout point dans l'élément
                if p in ids:                   #         si le point est sur la facette
                    rev[p].append(i)           #             ajouter l'élément à la liste associée au point

        if   name in ["D", "E", "S"]: field = self.compute_invariant(dates, name)
        elif name == "W"            : field = self.compute_work(dates)
        elif name == "WR"           : field = self.compute_work_rate(dates)
        else                        : field = self.read_fields(dates, [name])
        
        nodal = np.swapaxes(np.asarray([field[:,rev[idp]].sum(axis=1)/len(rev[idp]) for idp in ids], dtype=np.float64), axis1=1, axis2=0)

        return nodal


    def __plot_scalar_field(self, dates: list, name: str, savepath: str = "") -> None:
        """Plots or exports the picture of the scalar field named after the list ["D", "E", "S", "W", "WR", "P"].
        This algorithm gives a mesh with many holes, prefere the second method.

        Params
        ------
            dates    : list(int)
            name     : str
            savepath : str

        Returns
        -------
            None, if savepath is "" then plots the figure in the screen.
        """
        coor = self.read_coords(dates, ['x', 'z'])
        conn = self.read_elements()
        face = self.read_faces()

        ids_ = np.where(face[:,0]==1)[0]
        ids  = np.unique(face[ids_,1:4].flatten())
        for i, idp in enumerate(ids):
            face[:,1:4] = np.where(face[:,1:4]==idp, i, face[:,1:4])
        vals = []                              # liste des valeurs du dictionnaire
        for _ in range(len(ids)):              # pour tout indice de noeud sur la facette 1...
            vals.append([])                    # ... créer une liste vide
        rev = dict( zip(ids,vals) )            # créer le dictionnaire qui à chaque indice de noeud de la facette 1 associe la liste vide

        for i, el in enumerate(conn[:,1:]):    # pour tout élément
            for p in el:                       #     pour tout point dans l'élément
                if p in ids:                   #         si le point est sur la facette
                    rev[p].append(i)           #             ajouter l'élément à la liste associée au point

        if   name in ["D", "E", "S"]: field = self.compute_invariant(dates, name)
        elif name == "W"            : field = self.compute_work(dates)
        elif name == "WR"           : field = self.compute_work_rate(dates)
        else                        : field = self.read_fields(dates, [name])

        nodal = np.swapaxes(np.asarray([field[:,rev[idp]].sum(axis=1)/len(rev[idp]) for idp in ids], dtype=np.float64), axis1=1, axis2=0)

        for i, n in enumerate(dates):
            mesh    = tri.Triangulation(coor[i,ids-1,0], coor[i,ids-1,1], triangles=face[ids-1,1:4])
            plt.triplot(mesh)
            plt.tricontourf(mesh, nodal[i,:])
            if os.path.isdir(savepath): plt.savefig(f"{savepath}/{name}/name_{str(n).zfill(3)}.jpg")
            else                      : plt.show()

        return None
    
    def plot_scalar_field(self, dates: list, name: str, savepath: str = "") -> None:
        """Plots or exports the picture of the scalar field named after the list ["D", "E", "S", "W", "WR", "P"].

        Params
        ------
            dates    : list(int) ; 
            name     : str       ; 
            savepath : str       ; 

        Returns
        -------
            None, if savepath is "" then plots the figure in the screen.
        """
        coor = self.read_coords(dates, ['x', 'z'])
        conn = self.read_elements()
        face = self.read_faces()

        ids_ = np.where(face[:,0]==1)[0]
        ids  = np.unique(face[ids_,1:4].flatten())
        vals = []                              # liste des valeurs du dictionnaire
        for _ in range(len(ids)):              # pour tout indice de noeud sur la facette 1...
            vals.append([])                    # ... créer une liste vide
        rev = dict( zip(ids,vals) )            # créer le dictionnaire qui à chaque indice de noeud de la facette 1 associe la liste vide

        for i, el in enumerate(conn[:,1:]):    # pour tout élément
            for p in el:                       #     pour tout point dans l'élément
                if p in ids:                   #         si le point est sur la facette
                    rev[p].append(i)           #             ajouter l'élément à la liste associée au point

        if   name in ["D", "E", "S"]: field = self.compute_invariant(dates, name)
        elif name == "W"            : field = self.compute_work(dates)
        elif name == "WR"           : field = self.compute_work_rate(dates)
        else                        : field = self.read_fields(dates, [name])

        nodal = np.swapaxes(np.asarray([field[:,rev[idp]].sum(axis=1)/len(rev[idp]) for idp in ids], dtype=np.float64), axis1=1, axis2=0)

        for i, n in enumerate(dates):
            mesh    = tri.Triangulation(coor[i,ids-1,0], coor[i,ids-1,1])
            plt.triplot(mesh)
            plt.tricontourf(mesh, nodal[i,:])
            if os.path.isdir(savepath): plt.savefig(f"{savepath}/{name}/name_{str(n).zfill(3)}.jpg")
            else                      : plt.show()

        return None
    

    def interpolate_on_regular_grid(self, dates: list, name: str, Nx: int) -> np.ndarray:
        """
        """
        conn = self.read_elements()
        face = self.read_faces()
        ids_  = np.where(face[:,0]==1)[0]
        ids  = np.unique(face[ids_,1:4].flatten())

        vals = []                              # liste des valeurs du dictionnaire
        for _ in range(len(ids)):              # pour tout indice de noeud sur la facette 1...
            vals.append([])                    # ... créer une liste vide
        rev = dict( zip(ids,vals) )            # créer le dictionnaire qui à chaque indice de noeud de la facette 1 associe la liste vide

        for i, el in enumerate(conn[:,1:]):    # pour tout élément
            for p in el:                       #     pour tout point dans l'élément
                if p in ids:                   #         si le point est sur la facette
                    rev[p].append(i)           #             ajouter l'élément à la liste associée au point

        if   name in ["D", "E", "S"]: field = self.compute_invariant(dates, name)
        elif name == "W"            : field = self.compute_work(dates)
        elif name == "WR"           : field = self.compute_work_rate(dates)
        else                        : field = self.read_fields(dates, [name])
        
        nodal = np.swapaxes(np.asarray([field[:,rev[idp]].sum(axis=1)/len(rev[idp]) for idp in ids], dtype=np.float64), axis1=1, axis2=0)

        coor = self.read_coords(dates, ['x','z'])

        interp = []
        for i, n in enumerate(dates):
            xlin = np.linspace(coor[i,ids-1,0].min(), coor[i,ids-1,0].max(), Nx)
            zlin = np.linspace(coor[i,ids-1,1].min(), coor[i,ids-1,1].max(), Nx)
            xlin, zlin = np.meshgrid(xlin, zlin)
            interp.append(sp.interpolate.griddata( (coor[i,ids-1,0],coor[i,ids-1,1]), nodal[i,:], (xlin,zlin), method='cubic' ))

        interp = np.asarray(interp)

        return interp
    

    def analysis(self, dates: list, name: str, savepath: str = None) -> None:
        """Analyses the final state of a simulation with horizontal shear as boundary conditions.
        The method may not be adapted for other types of simulations, to check.
        Issues : the threshold to binarise is arbitrary.

        Params
        ------
            dates    : list(int) ; list of indices of output snapshoots.
            name     : str       ; name of the field to analyse, Peierls here.
            savepath : str, opt. ; the path to the folder in which the result will be stored.

        Returns
        -------
            None.   
        """
        Nx    = 256
        data_ = self.interpolate_on_regular_grid(dates, name, Nx)
        
        for i, n in enumerate(dates):
            data    = data_[i,...,0]
            data    = data/np.nanmax(data)
            bin     = np.nan_to_num(data, nan=1.0)
            bin     = np.where(bin > 0.5, 0.0, 1.0)
            slice1  = np.convolve(bin[:,Nx//2-Nx//6], np.ones(10)/10.0, mode='same')
            peaks1, info1 = sp.signal.find_peaks(slice1, width=1)
            slice2  = np.convolve(bin[:,Nx//2+Nx//6], np.ones(10)/10.0, mode='same')
            peaks2, info2 = sp.signal.find_peaks(slice2, width=1)
            
            width1  = np.abs(info1["left_ips"] - info1["right_ips"])
            width2  = np.abs(info2["left_ips"] - info2["right_ips"])
            widths  = (width1 + width2)/2.0

            dy      = np.abs(peaks1 - peaks2)
            dx      = 2*Nx//6
            angles  = np.arctan(dy/dx)*180/np.pi

            nrows = 1
            ncols = 4
            fig   = plt.figure(figsize=(ncols*12,nrows*10))

            axe   = fig.add_subplot(nrows, ncols, 1)
            draw  = axe.imshow(data, aspect="auto", origin="lower")
            plt.colorbar(draw)

            axe   = fig.add_subplot(nrows, ncols, 2, aspect="auto")
            draw  = axe.imshow(bin, aspect="auto", origin="lower")
            axe.vlines(x=Nx//2-Nx//6, ymin=0, ymax=Nx, color="C1")
            axe.vlines(x=Nx//2+Nx//6, ymin=0, ymax=Nx, color="C2")
            plt.colorbar(draw)

            axe   = fig.add_subplot(nrows, ncols, 3, aspect="auto")
            axe.plot(slice1, color="C1")
            axe.plot(slice2, color="C2")
            axe.scatter(peaks1, slice1[peaks1], color="C1")
            axe.scatter(peaks2, slice2[peaks2], color="C2", marker='x')

            axe   = fig.add_subplot(nrows, ncols, 4, aspect="auto")
            axe.axis('off')
            axe.text(x=0, y=0.00, s=f"Largeur des bandes : {widths} [px]")
            axe.text(x=0, y=0.05, s=f"Orientations       : {angles} [°]")
            axe.text(x=0, y=0.10, s=f"Nombre de bandes   : {len(peaks1)}")
            
            if savepath == None:
                plt.show()
            else:
                fig.savefig(f"{savepath}/final_analysis.jpg")

        return None
    
    def analysis2(self, dates: list, name: str, savepath: str = None) -> None:
        """
        """
        Nx    = 256
        data_ = self.interpolate_on_regular_grid(dates, name, Nx)

        for i, n in enumerate(dates):
            data    = data_[i,...,0]
            data    = data/np.nanmax(data)
            data    = np.nan_to_num(data, nan=1.0)
            eps     = 0.1*(data.max()-data.min())
            minx, miny = np.where(data < data.min()+eps)

            nrows = 1
            ncols = 1
            fig   = plt.figure(figsize=(ncols*12,nrows*10))

            axe   = fig.add_subplot(nrows, ncols, 1)
            draw  = axe.imshow(data, aspect="auto", origin="lower")
            axe.scatter(minx, miny, color="C1")
            plt.colorbar(draw)

            plt.show()
        return None


    def __autocorrelation_analysis(self, dates: list, name: str, savepath: str = None, plot: bool = True) -> float:
        """Performs the characterisation of the localisation of physical fields in bands systems through autocorrelation
        analysis. Deprecated : the new version does not call an other script but implements directly useful functions.

        Params
        ------

        Returns
        -------
            lengths : np.ndarray, dim=1 ; 
        """
        data    = self.interpolate_on_regular_grid(dates, name, 256)
        lengths = np.zeros_like(dates)

        for i, n in enumerate(dates):
            deg, dis, auto, cinf, peaks, info = image.autocorrelation_analysis(data[i,...,0])
            rad = np.linspace(0, 2*np.pi, 360)
            dis = np.hstack([dis, dis])
            lengths[i] = dis.max()

            if plot:
                nrows = 1
                ncols = 3
                fig   = plt.figure(figsize=(ncols*10, nrows*10))

                axe  = fig.add_subplot(nrows, ncols, 1)
                draw = axe.imshow(data[i,...], origin="lower")
                axe.set_title(f"Champs", fontsize=20)
                axe.set_xlabel("X [px]", fontsize=15)
                axe.set_ylabel("Z [px]", fontsize=15)
                plt.colorbar(draw)

                axe  = fig.add_subplot(nrows, ncols, 2)
                draw = axe.imshow(auto, origin="lower")
                axe.contour(auto, [cinf, 1.0])
                axe.set_title(f"Autocorrelation", fontsize=20)
                axe.set_xlabel("X [px]", fontsize=15)
                axe.set_ylabel("Z [px]", fontsize=15)
                plt.colorbar(draw)

                axe = fig.add_subplot(nrows, ncols, 3, projection="polar")
                axe.fill(rad, dis, color="tan")
                axe.set_rmax(dis.max())
                axe.set_title(f"Distance à Cinf en fonction de l'angle", fontsize=15)

                fig.suptitle(f"Snap id : {n}", fontsize=40)

                if savepath == None: 
                    plt.show()
                else: 
                    plt.savefig(f"{savepath}/autocorrelation_{str(n).zfill(3)}.jpg")
        
        return lengths
    
    def autocorrelation_analysis(self, dates: list, name: str, savepath: str = None, plot: bool = True) -> np.ndarray:
        """Performs the characterisation of the localisation of physical fields in bands systems through autocorrelation
        analysis.

        Params
        ------

        Returns
        -------
            lengths : np.ndarray, dim=1 ; 
        """
        def autocorrelation(data: np.ndarray) -> np.ndarray:
            """
            """
            data_ = np.fft.fft2(data)
            data_ = np.power(np.abs(data_), 2)
            data_ = np.fft.ifft2(data_)
            data_ = np.abs( np.fft.fftshift(data_) / np.nanmax(data_) )
            return data_
        
        def cinfinity(data: np.ndarray) -> float:
            """
            """
            if np.allclose(data, np.zeros_like(data)):
                c = 0.0
            else:
                c = np.power(np.nanmean(data), 2) / np.nanmean(np.power(data, 2))
            return c

        def length_vs_angle(data: np.ndarray, cinf: float, angle: float) -> float:
            """Detects the first contour with value cinf, starting from the center
            of the picture, along the axis with angle `angle`, in radians.

            Params
            ------
                data  : np.ndarray, dim=2 ; autocorrelation image
                cinf  : float             ; value to detect
                angle : float             ; angle of the axis, in rad
            
            Returns
            -------
                l : float ; length of the segment starting from the center to the
                            the first level cinf
            """
            N = data.shape[0]

            while(angle < 0      ): angle = angle + 2*np.pi
            while(angle > 2*np.pi): angle = angle - 2*np.pi
            if    angle > np.pi   : angle = angle - np.pi
            if angle == 0:
                j_ = np.linspace(0, N/2-1, N//2).astype(np.int64)
                i_ = np.zeros_like(j_).astype(np.int64)
            elif angle == np.pi/2:
                i_ = np.linspace(0, N/2-1, N//2).astype(np.int64)
                j_ = np.zeros_like(i_).astype(np.int64)
            elif angle <= np.pi/4 or angle > 3*np.pi/4:
                j_ = np.linspace(0, N/2-1, N//2).astype(np.int64)
                i_ = np.round(j_ * np.tan(angle)).astype(np.int64)
                j_ = j_[np.abs(i_)<N//2]
                i_ = i_[np.abs(i_)<N//2]
            elif angle > np.pi/4 or angle <= 3*np.pi/4:
                i_ = np.linspace(0,N/2-1, N//2).astype(np.int64)
                j_ = np.round(i_/np.tan(angle)).astype(np.int64)
                i_ = i_[np.abs(j_)<N//2]
                j_ = j_[np.abs(j_)<N//2]
            l  = np.sqrt( i_.max()**2 + j_.max()**2 )
            for (i, j) in zip(i_, j_):
                if data[N//2+i, N//2+j] > cinf:
                    continue
                else:
                    l = np.sqrt(i**2 + j**2)
                    break
            return l

        def radial_profile(auto: np.ndarray, angle: float) -> tuple:
            """Extracts the values of the autocorrelation along a line with an angle.

            Params
            ------
                auto : np.ndarray, dim=2 ; the autocorrelation table
                angle: float             ; the angle with the horizontal, in rad

            Returns
            -------
                r : np.ndarray, dim=1 ; table of length, radius
                a : np.ndarray, dim=1 ; values of the autocorrelation along the line
            """
            N = auto.shape[0]

            while(angle < 0      ): angle = angle + 2*np.pi
            while(angle > 2*np.pi): angle = angle - 2*np.pi
            if    angle > np.pi   : angle = angle - np.pi
            if angle == 0:
                j_ = np.linspace(0, N/2-1, N//2).astype(np.int64)
                i_ = np.zeros_like(j_).astype(np.int64)
            elif angle == np.pi/2:
                i_ = np.linspace(0, N/2-1, N//2).astype(np.int64)
                j_ = np.zeros_like(i_).astype(np.int64)
            elif angle <= np.pi/4 or angle > 3*np.pi/4:
                j_ = np.linspace(0, N/2-1, N//2).astype(np.int64)
                i_ = np.round(j_ * np.tan(angle)).astype(np.int64)
                j_ = j_[np.abs(i_)<N//2]
                i_ = i_[np.abs(i_)<N//2]
            elif angle > np.pi/4 or angle <= 3*np.pi/4:
                i_ = np.linspace(0, N/2-1, N//2).astype(np.int64)
                j_ = np.round(i_/np.tan(angle)).astype(np.int64)
                i_ = i_[np.abs(j_)<N//2]
                j_ = j_[np.abs(j_)<N//2]
            r  = np.sqrt( np.power(i_, 2) + np.power(j_, 2) )
            a  = np.zeros_like(r)
            for k in range(len(r)):
                if angle < np.pi/2:
                    a[k] = auto[N//2-i_[k], N//2+j_[k]]
                else:
                    a[k] = auto[N//2-i_[k], N//2-j_[k]]
            return (r, a)
    
        deg     = np.linspace(0, 179, 180).astype(np.int32)    # degrees from 0 to 179 as integers
        rad     = deg * np.pi / 180.                           # radians between 0 and almost pi as floats
        lengths = np.zeros_like(dates, dtype=np.float64)
        field   = self.interpolate_on_regular_grid(dates, name, 256)
        
        for i, n in enumerate(dates):
            dis  = np.zeros_like(rad) *np.nan               # distance from the center to the first contour cinf, in pixels
            data = field[i,...]
            data = np.nan_to_num(data, nan=np.nanmean(data))

            # autocorrelation
            auto = autocorrelation(data)
            cinf = cinfinity(data)

            # distance to Cinf
            for k in range(len(rad)):
                dis[k] = length_vs_angle(auto, cinf, rad[k])
            lengths[i] = dis.max()

            # peaks characterisation
            peaks, info = sp.signal.find_peaks( dis,
                                                distance=None,
                                                height=None,
                                                plateau_size=None,
                                                prominence=None,
                                                rel_height=None,
                                                threshold=None, 
                                                width=None,
                                                wlen=None )
            if plot:
                nrows = 1
                ncols = 3
                fig   = plt.figure(figsize=(ncols*10, nrows*10))

                axe  = fig.add_subplot(nrows, ncols, 1)
                draw = axe.imshow(data, origin="lower")
                axe.set_title(f"Champs", fontsize=20)
                axe.set_xlabel("X [px]", fontsize=15)
                axe.set_ylabel("Z [px]", fontsize=15)
                plt.colorbar(draw)

                axe  = fig.add_subplot(nrows, ncols, 2)
                draw = axe.imshow(auto, origin="lower")
                axe.contour(auto, [cinf, 1.0])
                axe.set_title(f"Autocorrelation", fontsize=20)
                axe.set_xlabel("X [px]", fontsize=15)
                axe.set_ylabel("Z [px]", fontsize=15)
                plt.colorbar(draw)

                axe = fig.add_subplot(nrows, ncols, 3, projection="polar")
                rad = np.linspace(0, 2*np.pi, 360)
                dis = np.hstack([dis, dis])
                axe.fill(rad, dis, color="tan")
                axe.set_rmax(dis.max())
                axe.set_title(f"Distance à Cinf en fonction de l'angle", fontsize=15)

                fig.suptitle(f"Snap id : {n}", fontsize=40)

                if savepath == None: 
                    plt.show()
                else: 
                    plt.savefig(f"{savepath}/autocorrelation_{str(n).zfill(3)}.jpg")
            
        return lengths


    def gradient_analysis(self, dates: list, name: str, savepath: str = None) -> None:
        """
        """
        Nx   = 256
        data = self.interpolate_on_regular_grid(dates, name, Nx)

        for i, n in enumerate(dates):
            grad        = np.asarray(np.gradient(data[i,...]))
            gradn       = np.linalg.norm(grad, axis=0)
            sliced_data = np.convolve(data[i,:,Nx//2], np.ones(5)/5.0, mode='same')
            sliced_grad = np.convolve(gradn[:,Nx//2], np.ones(5)/5.0, mode='same')

            nrows = 1
            ncols = 3
            fig   = plt.figure(figsize=(ncols*12, nrows*10))

            axe  = fig.add_subplot(nrows, ncols, 1)
            draw = axe.imshow(data[i,...], aspect="auto", origin="lower")
            axe.vlines(x=Nx//2, ymin=0, ymax=Nx, color="C1")
            axe.set_title(r"$F(x,z)$", fontsize=20)
            axe.set_xlabel("X [px]", fontsize=10)
            axe.set_ylabel("Z [px]", fontsize=10, rotation="horizontal")
            plt.colorbar(draw)
            
            axe  = fig.add_subplot(nrows, ncols, 2)
            draw = axe.imshow(gradn, aspect="auto", origin="lower")
            axe.vlines(x=Nx//2, ymin=0, ymax=Nx, color="C2")
            axe.set_title(r"$|\nabla F|(x,z)$", fontsize=20)
            axe.set_xlabel("X [px]", fontsize=10)
            plt.colorbar(draw)

            axe = fig.add_subplot(nrows, ncols, 3)
            axe.plot(sliced_data, "-o", color="C1", markersize=2)
            axe.plot(sliced_grad, "-o", color="C2", markersize=2)

            if savepath == None: plt.show()
            else               : plt.savefig(f"{savepath}/gradient_{str(n).zfill(3)}.jpg")

        return None


    def spectral_analysis(self, dates: list, name: str, savepath: str = None) -> None:
        """
        """
        Nx    = 256
        data_ = self.interpolate_on_regular_grid(dates, name, Nx)

        for i, n in enumerate(dates):
            data = data_[i,...]
            data = np.nan_to_num(data, nan=np.nanmean(data))
            
            power     = np.abs(np.fft.fft2(np.fliplr(data)))
            logpower  = np.log(power)
            logpower /= np.nanmax(logpower)
            grad      = np.asarray(np.gradient(logpower))
            norm      = np.linalg.norm(grad, axis=0)
            norm     /= np.nanmax(norm)

            Nt         = 64
            thresholds = np.arange(0.0, 1.0+1/Nt, 1/Nt) *np.nanmax(logpower)
            cumulated  = np.zeros_like(thresholds)
            for i, thresh in enumerate(thresholds):
                cumulated[i] = len(np.where(logpower<thresh)[0])
            diff_cum   = np.diff(cumulated)
            diff2_cum  = np.diff(diff_cum)
            inflection = np.argmax(diff_cum)

            slice1  = np.convolve(logpower[:,Nx//2-Nx//6], np.ones(10)/10.0, mode='same')
            slice1  = np.where(slice1 < thresholds[inflection], 0.0, 1.0)
            peaks1, info1 = sp.signal.find_peaks(slice1, width=1)
            slice2  = np.convolve(logpower[:,Nx//2+Nx//6], np.ones(10)/10.0, mode='same')
            slice2  = np.where(slice2 < thresholds[inflection], 0.0, 1.0)
            peaks2, info2 = sp.signal.find_peaks(slice2, width=1)
            
            nrows = 2
            ncols = 3
            fig   = plt.figure(figsize=(ncols*10, nrows*10))

            axe  = fig.add_subplot(nrows, ncols, 1)
            draw = axe.imshow(data, aspect="auto", origin="lower")
            plt.colorbar(draw)

            axe  = fig.add_subplot(nrows, ncols, 2)
            draw = axe.imshow(logpower, aspect="auto", origin="lower")
            axe.vlines(x=Nx//2-Nx//6, ymin=0, ymax=Nx, color="C1")
            axe.vlines(x=Nx//2+Nx//6, ymin=0, ymax=Nx, color="C2")
            plt.colorbar(draw)

            axe  = fig.add_subplot(nrows, ncols, 3)
            draw = axe.imshow(norm, aspect="auto", origin="lower")
            plt.colorbar(draw)

            axe  = fig.add_subplot(nrows, ncols, 4)
            axe.plot(thresholds, cumulated, '-x', color="C1")
            axe.tick_params(axis='y', labelcolor="C1")
            axe.scatter(thresholds[inflection], cumulated[inflection], color="C4")
            axe  = axe.twinx()
            axe.plot(thresholds[:-1], diff_cum,   color="C2")
            axe.plot(thresholds[:-2], diff2_cum,  color="C3")
            axe.tick_params(axis='y', labelcolor="black")
            
            if n == len(dates)-1:
                axe  = fig.add_subplot(nrows, ncols, 5)
                draw = axe.contourf(logpower, [0.0, thresholds[inflection], np.nanmax(logpower)])
                plt.colorbar(draw)

            axe  = fig.add_subplot(nrows, ncols, 6)
            axe.plot(slice1, color="C1")
            axe.plot(slice2, color="C2")

            if savepath == None:
                plt.show()
            else               :
                fig.savefig(f"{savepath}/spectral_{str(n).zfill(3)}.jpg")

        return None
    

    def compute_heterogeneity_size(self) -> float:
        """Computes the characteristics length defined as the Peierls' initial heterogeneity.

        Params
        ------
            None

        Returns
        -------
            length : np.ndarray, dim=1 ; Peierls' initial heterogeneity size normalised by the total length
                                         of the physical domain.
        """
        dates  = [0]
        name   = "Peierls"
        length = self.autocorrelation_analysis(dates, name, plot=False)

        return length
    

    def compute_mean_variation_of_Peierls(self, dates: list) -> np.ndarray:
        """Computes the mean variation of Peierls field, normalised by the mean of the
        field.
        """
        array   = np.zeros_like(dates, dtype=np.float64)
        peierls = self.interpolate_on_regular_grid(dates, "Peierls", 256)[...,0]
        
        for i, n in enumerate(dates):
            peierls[i,...] = np.nan_to_num(peierls[i,...], nan=np.nanmean(peierls[i,...]))
            array[i] = np.linalg.norm(np.gradient(peierls[i,...])).mean()/peierls[i,...].mean()

        return array


if __name__ == "__main__":
    path   = "/home/julien/RhEoVOLUTION/data/adeli/beta07_rect_P2_0.5GPa_init1_neigh1_time10_dtup10000_500000_1000K_Zfree_WC_11_7_5e6_PNew1e9_long/pcompr"
    # path   = "/home/julien/RhEoVOLUTION/data/adeli/Ext_rect7x3fin_P2_0.5GPa_init0_neigh1_time10_dtup10000_500000_1000K_Zfree_WR3.1_PNew1.5e9_long/pcompr"
    source = Simulation(path)
    
    # dates  = [20]
    # names  = ["Peierls"]
    # name  = "E"
    print(f"> Test experience : {path}")
    print(f"  ------------------{len(path)*'-'}")
    # print()
    # print(f"> Available fields : {source.fields}")
    # print()
    # print("> Reading cells...")
    # misc.describe(source.read_elements())
    # print()
    # print("> Reading faces...")
    # misc.describe(source.read_faces())
    # print()
    # print("> Reading [X, Z] coordinates...")
    # misc.describe(source.read_coords(dates, ['x','z']))
    # print()
    # print("> Reading Peierls field...")
    # misc.describe(source.read_fields(dates, names))
    # print()
    # print("> Reading Peierls and viscosity at the same time...")
    # misc.describe(source.read_fields(dates, ["Peierls", "viscosite eff."]))
    # print()
    # print("> Reading deformation...")
    # misc.describe(source.read_deformation(dates))
    # print()
    # print("> Reading strain...")
    # misc.describe(source.read_strain(dates))
    # print()
    # print("> Reading stress...")
    # misc.describe(source.read_stress(dates))
    # print()
    # print("> Computing J2S...")
    # misc.describe(source.compute_invariant(dates, "S"))
    # print()
    # print("> Computing work...")
    # misc.describe(source.compute_work(dates))
    # print()
    # print("> Computing work rate...")
    # misc.describe(source.compute_work_rate(dates))
    # print()
    # print("> Plotting Peierls on the first face...")
    # source.plot_scalar_field(dates, "D")
    # print()
    # print("> Interpolation on a regular grid...")
    # misc.describe(source.interpolate_on_regular_grid(dates, "D", 128))
    
    # print()
    # print("> Simple analysis...")
    # source.analysis(dates, "Peierls", savepath="/home/julien/RhEoVOLUTION/lib/tmp")
    
    print()
    print("> Second kind analysis...")
    source.analysis2([20], "Peierls", savepath="/home/julien/RhEoVOLUTION/lib/tmp")

    # print()
    # print("> Autocorrelation analysis...")
    # source.autocorrelation_analysis(dates, name, savepath="/home/julien/RhEoVOLUTION/lib/tmp")
    # source.autocorrelation_analysis(dates, "E")
    
    # print()
    # print("> Gradient analysis...")
    # source.gradient_analysis(dates, name, savepath="/home/julien/RhEoVOLUTION/lib/tmp")
    # source.gradient_analysis(dates, "E")
    
    # print()
    # print("> Spectral analysis...")
    # source.spectral_analysis(dates, name, savepath="/home/julien/RhEoVOLUTION/lib/tmp")
    # source.spectral_analysis(dates, name)
    
    # print()
    # print("> Computing initial heterogeneity size...")
    # print(source.compute_heterogeneity_size())

    # print()
    # print("> Computing mean variation of Peierls...")
    # print(source.compute_mean_variation_of_Peierls(dates))

    print()
    print("> NORMAL END.")
