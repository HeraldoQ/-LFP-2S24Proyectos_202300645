import tkinter as tk
from tkinter import PhotoImage
from PIL import Image, ImageTk
import subprocess
from tkinter import messagebox
from tkinter import filedialog

ventana = tk.Tk()
ventana.title("Pantalla")
ventana.geometry("900x600")



def abrir():
    print("Abrir")
    global file_path
    file_path = filedialog.askopenfilename(filetypes=[("Org files", "*.org")])
    if file_path:
        with open(file_path, 'r') as file:
            content = file.read()
            entrada.delete("1.0", tk.END)
            entrada.insert(tk.END, content)

def guardar():
    print("Guardar")

    
    if file_path:
        with open(file_path, 'w') as file:
            content = entrada.get("1.0", tk.END)
            file.write(content)
    else:
        guardar_como()
def guardar_como():
    print("Guardar Como")

    file_path2 = filedialog.asksaveasfilename(defaultextension=".org", filetypes=[("Org files", "*.org")])
    if file_path2:
        with open(file_path2, 'w') as file:
            content = entrada.get("1.0", tk.END)
            file.write(content)



def enviar():


    data = entrada.get("1.0", tk.END)
    resultado = subprocess.run(
        ["./Proyecto2.exe"], 
        input=data,
        stdout=subprocess.PIPE,
        
        text=True
    )
    print(data)
    
    entrada.insert(tk.END,"\n")
    entrada.insert(tk.END, resultado.stdout)



#cuadro de texto de entrada
entrada = tk.Text(ventana, height=35, width=50, wrap = "word", font=("Calibri", 12))
entrada.pack(anchor = "w")



#############################################################################################################

#
#todo esto es el menú de la ventana
barra_menu = tk.Menu(ventana)

# Crear un menú "Menú"
menu_archivo = tk.Menu(barra_menu, tearoff=0)
menu_archivo.add_command(label="Abrir", command=abrir)
menu_archivo.add_command(label="Guardar", command=guardar)
menu_archivo.add_command(label="Guardar Como", command=guardar_como)
menu_archivo.add_separator()
menu_archivo.add_command(label="Salir", command=ventana.quit)
barra_menu.add_cascade(label="Menú", menu=menu_archivo)

# Crear un menú "Ayuda"
menu_ayuda = tk.Menu(barra_menu, tearoff=0)
menu_ayuda.add_command(label="Acerca de", command=lambda: messagebox.showinfo("Acerca de", "Carlos Heraldo Quiná Corona\n202300645"))
barra_menu.add_cascade(label="Ayuda", menu=menu_ayuda)

ventana.config(menu=barra_menu)






##########################################################################################################
#botones
boton = tk.Button(ventana, height=2, width=10, text="Ejecutar", command=enviar)
boton.place(relx=0.5, rely=0.5, anchor=tk.CENTER)


##########################################################################################################

#imagen del pais
# Crear un Frame para la imagen
frame_imagen1 = tk.Frame(ventana)
frame_imagen1.pack(anchor='e', pady=10)
frame_imagen1.place(relx=0.99, rely=0.6, anchor=tk.NE)


i1 = "\Flag_of_Guatemala (Grande).jpeg"



# Cargar la imagen usando Pillow
try:
    imagen1 = Image.open(r"C:\Cursos\Fortran\Lab_LFP\_LFP_2S24Proyectos_202300645\Proyecto1\banderas"+i1)
except:
    imagen1 = Image.open(r"C:\Cursos\Fortran\Lab_LFP\_LFP_2S24Proyectos_202300645\Proyecto1\banderas"+"\error.png")

# Redimensionar la imagen a las dimensiones deseadas (por ejemplo, 200x200)
imagen_redimensionada = imagen1.resize((226, 133))

# Convertir la imagen redimensionada a un objeto PhotoImage
imagen_tk1 = ImageTk.PhotoImage(imagen_redimensionada)

# Crear un Label dentro del Frame para mostrar la imagen
label_imagen = tk.Label(frame_imagen1, image=imagen_tk1)
label_imagen.pack()



#texto de la bandera
w = 5000
tk.Label(ventana, text="Pais: Guatemala", font=("Calibri", 12)).place(relx=0.6, rely=0.65, anchor=tk.CENTER)
tk.Label(ventana, text="Población: "+str(w), font=("Calibri", 12)).place(relx=0.6, rely=0.7, anchor=tk.CENTER)

##########################################################################################################

#imagen de graphviz
# Crear un Frame para la imagen
frame_imagen2 = tk.Frame(ventana)
frame_imagen2.pack(anchor='e', pady=10)
frame_imagen2.place(relx=0.99, rely=0, anchor=tk.NE)


i2 = "\Flag_of_Guatemala (Grande).jpeg"


# Cargar la imagen usando Pillow
try:
    imagen2 = Image.open(r"C:\Cursos\Fortran\Lab_LFP\_LFP_2S24Proyectos_202300645\Proyecto1\banderas"+i2)
except:
    imagen2 = Image.open(r"C:\Cursos\Fortran\Lab_LFP\_LFP_2S24Proyectos_202300645\Proyecto1\banderas"+"\error.png")


# Redimensionar la imagen a las dimensiones deseadas (por ejemplo, 200x200)
imagen_redimensionada = imagen2.resize((383, 256))

# Convertir la imagen redimensionada a un objeto PhotoImage
imagen_tk2 = ImageTk.PhotoImage(imagen_redimensionada)

# Crear un Label dentro del Frame para mostrar la imagen
label_imagens = tk.Label(frame_imagen2, image=imagen_tk2)
label_imagens.pack()


## def enviar():
#    data = entrada.get()
#guarda todo y lo muestra
ventana.mainloop()