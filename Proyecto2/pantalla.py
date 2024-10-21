import tkinter as tk
from tkinter import messagebox
from tkinter import filedialog
import subprocess
from tkinter.scrolledtext import ScrolledText
from tkinter import ttk

ventana = tk.Tk()
ventana.title("Pantalla")
ventana.geometry("1200x600")





def abrir():
    print("Abrir")
    global file_path
    file_path = filedialog.askopenfilename(filetypes=[("LFP files", "*.LFP"), ("All files", "*.*")])
    if file_path:
        with open(file_path, 'r', encoding='utf-8')  as file:
            content = file.read()
            entrada.delete("1.0", tk.END)
            entrada.insert(tk.END, content)
    else:
        guardar_como()

def Nuevo():
    print("Nuevo")
    for item in tree.get_children():
        tree.delete(item)
    entrada.delete("1.0", tk.END)
    file_path = ""

def guardar():
    print("Guardar")

    try:
        if file_path:
            with open(file_path, 'w') as file:
                content = entrada.get("1.0", tk.END)
                file.write(content)
    except NameError:
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
    entrada.delete("1.0", tk.END)

    entrada.insert(tk.END, resultado.stdout.strip())


    with open('C:\Cursos\Fortran\Lab_LFP\_LFP_2S24Proyectos_202300645\Proyecto2\errores.txt', 'r', encoding='utf-8') as file:
        lines = file.readlines()
        if lines and lines[0].strip() == "ccc/":
            lines = lines[1:]  # Leer desde la segunda línea si la primera está vacía
        contadorlineas = 0
        for i in range(1, int(len(lines)/4)+1):
            tree.insert("", tk.END, values=("Lexico",lines[contadorlineas], lines[contadorlineas+1], lines[contadorlineas+2], lines[contadorlineas+3]))
            contadorlineas += 4

    with open('C:\Cursos\Fortran\Lab_LFP\_LFP_2S24Proyectos_202300645\Proyecto2\erroresintacticos.txt', 'r', encoding='utf-8') as file:
        lines = file.readlines()
        if lines and lines[0].strip() == "ccc/":
            lines = lines[1:]  # Leer desde la segunda línea si la primera está vacía
        contadorlineas = 0
        for i in range(1, int(len(lines)/4)+1):
            tree.insert("", tk.END, values=("Sintactico",lines[contadorlineas], lines[contadorlineas+1], lines[contadorlineas+2], lines[contadorlineas+3]))
            contadorlineas += 4



#cuadro de texto de entrada
entrada = ScrolledText(ventana, height=50, width=70, wrap="word", font=("Lucida Console", 8), bd=2, relief="solid", undo = True)
entrada.pack(anchor="w")



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
boton = tk.Button(ventana, height=2, width=10, text="Enviar", command=enviar)
boton.place(relx=0.47, rely=0.04, anchor=tk.CENTER)

borrar = tk.Button(ventana, height=2, width=10, text="Nuevo", command=Nuevo)
borrar.place(relx=0.47, rely=0.15, anchor=tk.CENTER)




# Crear una tabla con ttk
columns = ["#1", "#2", "#3", "#4", "#5"]  # Columnas de la tabla

#si es un numero, se añade 1 al rango, porque el rango no incluye el ultimo numero, o algo asi



tree = ttk.Treeview(ventana, columns=columns, show="headings", height=20)  # Ajustar la altura de la tabla
columnas = int(666/len(columns))
# Ajustar el ancho de las columnas
for i in range(0, len(columns)):
    tree.column(columns[i], width=columnas, anchor=tk.CENTER)
tree.heading("#1", text="Tipo")
tree.heading("#2", text="Linea")
tree.heading("#3", text="Columna")
tree.heading("#4", text="Error")
tree.heading("#5", text="Tkn Esperado")






# Insertar datos de ejemplo en la tabla



# Empaquetar la tabla en la esquina inferior derecha
tree.place(relx=0.99, rely=0.99, anchor=tk.SE)


##########################################################################################################
ventana.mainloop()