module funciones
    implicit none


    Type :: clerrores
    integer :: numeroe
    character (len = 10):: errore
    integer :: columnae
    integer :: lineae
    character (len = 30) :: tkn_esperado
    end Type clerrores

    type(clerrores), allocatable :: listae(:)
    type(clerrores), allocatable :: listaesin(:)


    Type :: lexema
    integer :: numerol
    !si se trunca algo se hace desde aqui
    character(len = 79) :: lexemal
    character(len=25) :: tipol
    integer :: columnal
    integer :: lineal
    end Type lexema

    type(lexema), allocatable :: listal(:)


    Type :: objetoshtml
    character(len = 50) :: tipo
    character(len = 50) :: objeto

    end Type objetoshtml

    type(objetoshtml), allocatable :: listao(:)






    contains

    subroutine cerrores(st,numeroe, errore, columnae, lineae, tkn_esperado)
        type(clerrores), intent(out) :: st
        integer :: numeroe
        character  :: errore
        integer :: columnae
        integer  :: lineae
        character(len = *) :: tkn_esperado


        st%numeroe = numeroe
        st%errore = errore
        st%columnae = columnae
        st%lineae = lineae
        st%tkn_esperado = tkn_esperado

    


    end subroutine cerrores

    subroutine clexema(st,numerol, lexemal, tipol, lineal, columnal)
        type(lexema), intent(out) :: st
        integer :: numerol
        character(len =*)  :: lexemal
        character(len = *) :: tipol
        integer :: columnal
        integer  :: lineal

        st%numerol = numerol
        st%lexemal = lexemal
        st%tipol = tipol
        st%columnal = columnal
        st%lineal = lineal

    end subroutine clexema

    subroutine cobjetos(st, tipo, objeto)
        type(objetoshtml), intent(out) :: st
        character(len =*) :: tipo
        character(len =*) :: objeto

        st%tipo = tipo
        st%objeto = objeto

    end subroutine cobjetos


    


    subroutine agregarlexema(numlex, lexema, tipo, linea, columna)
        integer, intent(inout) :: numlex
        character(len = *), intent(in) :: lexema
        character(len = *), intent(in) :: tipo
        integer, intent(in) :: linea
        integer, intent(inout) :: columna

        numlex = numlex + 1

        listal(numlex)%numerol = numlex
        listal(numlex)%lexemal = trim(lexema)
        listal(numlex)%tipol =trim(tipo)
        listal(numlex)%lineal = linea
        listal(numlex)%columnal = columna

        columna = columna + 1


    end subroutine agregarlexema

    subroutine agregarerror( numerrores, linea, columna, token, tkn_esperado)
        integer, intent(inout) :: numerrores
        integer, intent(inout) :: linea
        integer, intent(inout) :: columna
        character(len = *), intent(in) :: token
        character(len = *), intent(in) :: tkn_esperado

        numerrores = numerrores + 1

        listae(numerrores)%numeroe = numerrores
        listae(numerrores)%lineae = linea
        listae(numerrores)%columnae = columna
        listae(numerrores)%errore = trim(token)
        listae(numerrores)%tkn_esperado = trim(tkn_esperado)

        columna = columna + 1

    end subroutine agregarerror


    subroutine agregarerrors( numerrores, linea, columna, token, tkn_esperado)
        integer, intent(inout) :: numerrores
        integer, intent(inout) :: linea
        integer, intent(inout) :: columna
        character(len = *), intent(in) :: token
        character(len = *), intent(in) :: tkn_esperado

        numerrores = numerrores + 1

        listaesin(numerrores)%numeroe = numerrores
        listaesin(numerrores)%lineae = linea
        listaesin(numerrores)%columnae = columna
        listaesin(numerrores)%errore = trim(token)
        listaesin(numerrores)%tkn_esperado = trim(tkn_esperado)

        

    end subroutine agregarerrors







    





end module funciones













program name
    use funciones
    implicit none
    

    !variables
        !variables para leer la entrada
        character(len=200) :: linea
        character(len=20000) :: entrada
        integer :: ios, len

        !variables para el parser
        integer :: puntero, lineaa, columnaa
        character(len=1) :: char
        integer :: estado

        !variables para el analizador lexico
        character(len=1), dimension(26) :: mayusculas, minusculas
        character(len=50) :: tkn
        character(len=2) :: comentario1Linea
        character(len=2) :: comentarioMultilineainicio, comentarioMultilineafin
        logical :: comentarioabierto = .false.
        character(len=10), dimension(8)  :: objetos
        character(len=11), dimension(3) :: estructuras
        character(len=13), dimension(8) :: propiedades
        character( len=11), dimension(2) :: colocacion
        logical :: controlesabierto = .false., propiedadesabierto = .false., colocacionabierto = .false.

        character(len=4) :: inicioestructura, finestructura
        logical :: estructuraabierto = .false.

        !character(len=200) :: palabra
        integer :: numerrores
        integer, dimension(100,4) :: errores


        !variables para los errores
        integer :: i
        character(len=1) :: char_error

        !variables para la tabla de errores
        character(len=300) :: errorest
       


        !contadores para analizador lexico
        !contadorestructura sirve para contar los caracteres de la estructura
        !numlexema sirve para cuando se añada a la lista de lexemas, siga un orden
        integer :: contadorestructura, numlexema




        !variables para analizador sintactico
        integer :: contadorlexico, estadolexico, numerroresintactico
        integer, dimension(100,4) :: erroresintactico
        logical :: rev1,rev2,rev3,rev4,rev5,rev6,rev7




        !variables finales para lo de html
        integer :: numvarhtml
        character(len=150) :: linkhtml










!/////////////////////////establecer tokens admitidos/////////////////////////////////
    mayusculas = ['A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'I', 'J', 'K', 'L', 'M', 'N', 'O', 'P', 'Q', 'R', 'S', 'T', 'U', 'V', &
    'W', 'X', 'Y', 'Z']
    minusculas = ['a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k', 'l', 'm', 'n', 'o', 'p', 'q', 'r', 's', 't', 'u', 'v', &
    'w', 'x', 'y', 'z']
    comentario1Linea = '//'
    comentarioMultilineainicio = '/*'
    comentarioMultilineafin = '*/'

    objetos = ['Etiqueta  ', 'Boton     ', 'Check     ', 'RadioBoton', 'Texto     ', 'AreaTexto ', 'Clave     ', 'Contenedor']
    estructuras = ['Controles  ', 'propiedades', 'Colocacion ']
    propiedades = ['setColorLetra', 'setTexto     ', 'setAlineacion', 'setColorFondo', 'setMarcada   ', 'setGrupo     ',&
    'setAncho     ', 'setAlto      ']
    colocacion = ['setPosicion', 'add        ']

    inicioestructura = '<!--'
    finestructura = '-->'



!/////////////////////////establecer listas/////////////////////////////////
    allocate(listae(100))
    allocate(listal(500))
    allocate(listaesin(100))





!/////////////////////////Establecer Valores para el parser///////////////////////////
    puntero = 1
    columnaa = 0
    lineaa = 1
    estado = 0
    numlexema = 0
    tkn = ""

    !/////////////////////////Establecer Valores para el leer la entrada///////////////////////////
    entrada = ""

!///////////////////////// valores para los contadores del analizador lexico///////////////////////////
    
    contadorestructura = 0

!///////////////////////// Establecer valores para el lexico///////////////////////////
contadorlexico = 0
estadolexico = 0
numerroresintactico = 0

rev1 = .false.
rev2 = .false.
rev3 = .false.
rev4 = .false.
rev5 = .false.
rev6 = .false.
rev7 = .false.

!/////////////////////////Establecer valores para obj html///////////////////////////
numvarhtml = 0



    do
        
        read(*,'(A)', iostat=ios) linea
        if (ios /= 0) exit
        if (trim(linea) == "|") exit
        if (entrada /= "") then
            
            entrada = trim(entrada) // new_line('a') // trim(linea)
        
        
        else
            entrada = trim(linea)
        end if 
        
    end do

    len = len_trim(entrada)
    !print *, "entrada: ", trim(entrada)



 !/////////////////////////analizador lexico///////////////////////////

    do while (puntero <=len)
        
            char = entrada(puntero:puntero)
            !print *, trim(char)
        
            if ((ichar(char) == 10) .and. (estructuraabierto .eqv. .false.) .and. (comentarioabierto .eqv. .false.)) then
                lineaa = lineaa + 1
                columnaa = 0
                puntero = puntero + 1
                !print *, "salto de lineas"
            elseif (ichar(char) == 9) then
                columnaa = columnaa + 1
                puntero = puntero + 1
                !print *, "tabulador"
            elseif ((ichar(char) == 32) .and. (controlesabierto .eqv. .false.)) then
                if (any(trim(tkn) == objetos)) then
                
                endif
                columnaa = columnaa + 1
                puntero = puntero + 1
                !print *, "espacio"
            elseif (ichar(char) == 00 .or. ichar(char) == 11) then
                columnaa = columnaa + 1
                puntero = puntero + 1
                !print *, "caracter nulo"
            else
                
                select case (estado)
                case (0)
                    if (estructuraabierto .eqv. .false.) then
                        tkn = trim(tkn) // char
                        columnaa = columnaa + 1
                        contadorestructura = contadorestructura + 1
                        if (contadorestructura == 4) then
                            if (tkn == inicioestructura) then
                                columnaa = columnaa - 1
                                estructuraabierto = .true.
                                contadorestructura = 0
                                call agregarlexema(numlexema, tkn, "Inicio de estructura", lineaa, columnaa)
                                !!print *, "tkn: ", tkn
                                tkn = ""

                            else
                                call agregarerror(numerrores, lineaa, columnaa, trim(tkn), inicioestructura)
                                tkn = ""
                                contadorestructura = 0
                                print *, "error"

                            end if
                        elseif (contadorestructura == 2) then
                                if(tkn == comentario1Linea) then
                                    comentarioabierto = .true.
                                    contadorestructura = 0
                                    tkn = ""
                                    estado = 4
                                elseif (tkn == comentarioMultilineainicio) then
                                    comentarioabierto = .true.
                                    contadorestructura = 0
                                    tkn = ""
                                    estado = 5
                                
                                
                                endif
                        end if
                    elseif (estructuraabierto .eqv. .true. .and. controlesabierto .eqv. .false. .and. &
                        propiedadesabierto .eqv. .false. .and. colocacionabierto .eqv. .false.) then
                        
                        if (ichar(char) == 10) then
                            
                            if (any(tkn == estructuras)) then
                                if (tkn == estructuras(1)) then
                                    controlesabierto = .true.
                                    call agregarlexema(numlexema, tkn, "Inicio de controles", lineaa, columnaa)
                                    !!print *, "tkn: ", tkn
                                    tkn = ""
                                    estado = 1
                                    puntero = puntero + 1
                                    lineaa = lineaa + 1
                                    columnaa = 0
                                    cycle

                                elseif (tkn == estructuras(2)) then
                                    propiedadesabierto = .true.
                                    call agregarlexema(numlexema, tkn, "Inicio de propiedades", lineaa, columnaa)
                                    !!print *, "tkn: ", tkn
                                    tkn = ""
                                    estado = 2
                                    puntero = puntero + 1
                                    lineaa = lineaa + 1
                                    columnaa = 0
                                    cycle

                                elseif (tkn == estructuras(3)) then
                                    colocacionabierto = .true.
                                    call agregarlexema(numlexema, tkn, "Inicio de colocacion", lineaa, columnaa)
                                    !!print *, "tkn: ", tkn
                                    tkn = ""
                                    estado = 3
                                    puntero = puntero + 1
                                    lineaa = lineaa + 1
                                    columnaa = 0
                                    cycle

                                end if
                            
                            

                            else
                                call agregarerror(numerrores, lineaa, columnaa, trim(tkn), estructuras(1))
                                print *, "error"
                                columnaa = columnaa + 1
                                tkn = ""
                                lineaa = lineaa + 1
                                
                                columnaa = 0
                            
                            endif
                            
                        end if
                        tkn = trim(tkn) // char
                        columnaa = columnaa + 1
                    
                    elseif (controlesabierto .eqv. .true.) then
                        estado = 1
                    elseif (propiedadesabierto .eqv. .true.) then
                        estado = 2
                    elseif (colocacionabierto .eqv. .true.) then
                        estado = 3
                    end if
                    
                case (1)
                    !estado para cuando este activada la estructura de controles
                    
                    !print *, "estado 1"
                    !ichar 10 es el salto de linea
                    if ((char /= ' ') .and. (char /= ';') .and. (ichar(char) /= 10)) then
                        tkn = trim(tkn) // char
                        contadorestructura = contadorestructura + 1
                        columnaa = columnaa + 1
                    elseif (char == ' ') then
                        columnaa = columnaa + 1
                        contadorestructura = 0
                    elseif (ichar(char) == 10) then
                        if (tkn /= "") then
                            if (tkn == objetos(1)) then
                                call agregarlexema(numlexema, tkn, "Etiqueta", lineaa, columnaa)
                                !!print *, "tkn: ", tkn
                                tkn = ""
                                contadorestructura = 0
                            elseif (tkn == objetos(2)) then
                                call agregarlexema(numlexema, tkn, "Boton", lineaa, columnaa)
                                !!print *, "tkn: ", tkn
                                tkn = ""
                                contadorestructura = 0
                            elseif (tkn == objetos(3)) then
                                call agregarlexema(numlexema, tkn, "Check", lineaa, columnaa)
                                !!print *, "tkn: ", tkn
                                tkn = ""
                                contadorestructura = 0
                            elseif (tkn == objetos(4)) then
                                call agregarlexema(numlexema, tkn, "RadioBoton", lineaa, columnaa)
                                !!print *, "tkn: ", tkn
                                tkn = ""
                                contadorestructura = 0
                            elseif (tkn == objetos(5)) then
                                call agregarlexema(numlexema, tkn, "Texto", lineaa, columnaa)
                                !!print *, "tkn: ", tkn
                                tkn = ""
                                contadorestructura = 0
                            elseif (tkn == objetos(6)) then
                                call agregarlexema(numlexema, tkn, "AreaTexto", lineaa, columnaa)
                                !!print *, "tkn: ", tkn
                                tkn = ""
                                contadorestructura = 0
                            elseif (tkn == objetos(7)) then
                                call agregarlexema(numlexema, tkn, "Clave", lineaa, columnaa)
                                !!print *, "tkn: ", tkn
                                tkn = ""
                                contadorestructura = 0
                            elseif (tkn == objetos(8)) then
                                call agregarlexema(numlexema, tkn, "Contenedor", lineaa, columnaa)
                                !!print *, "tkn: ", tkn
                                tkn = ""
                                contadorestructura = 0
                            elseif (tkn == propiedades(1)) then
                                call agregarlexema(numlexema, tkn, "setColorLetra", lineaa, columnaa)
                                !!print *, "tkn: ", tkn
                                tkn = ""
                                contadorestructura = 0  
                            elseif (tkn == propiedades(2)) then
                                call agregarlexema(numlexema, tkn, "setTexto", lineaa, columnaa)
                                !!print *, "tkn: ", tkn
                                tkn = ""
                                contadorestructura = 0
                            elseif (tkn == propiedades(3)) then
                                call agregarlexema(numlexema, tkn, "setAlineacion", lineaa, columnaa)
                                !!print *, "tkn: ", tkn
                                tkn = ""
                                contadorestructura = 0
                            elseif (tkn == propiedades(4)) then
                                call agregarlexema(numlexema, tkn, "setColorFondo", lineaa, columnaa)
                                !!print *, "tkn: ", tkn
                                tkn = ""
                                contadorestructura = 0
                            elseif (tkn == propiedades(5)) then
                                call agregarlexema(numlexema, tkn, "setMarcada", lineaa, columnaa)
                                !!print *, "tkn: ", tkn
                                tkn = ""
                                contadorestructura = 0
                            elseif (tkn == propiedades(6)) then
                                call agregarlexema(numlexema, tkn, "setGrupo", lineaa, columnaa)
                                !!print *, "tkn: ", tkn
                                tkn = ""
                                contadorestructura = 0
                            elseif (tkn == propiedades(7)) then
                                call agregarlexema(numlexema, tkn, "setAncho", lineaa, columnaa)
                                !!print *, "tkn: ", tkn
                                tkn = ""
                                contadorestructura = 0
                            elseif (tkn == propiedades(8)) then
                                call agregarlexema(numlexema, tkn, "setAlto", lineaa, columnaa)
                                !!print *, "tkn: ", tkn
                                tkn = ""
                                contadorestructura = 0
                            else
                                if (ichar(char) == 10) then
                                    call agregarerror(numerrores, lineaa, columnaa, "/n", "ID")
                                    print *, "error"
                                    tkn = ""
                                else
                                    call agregarerror(numerrores, lineaa, columnaa, trim(tkn), "ID")
                                    print *, "error"
                                    tkn = ""
                                endif
                            end if
                        

                        end if
                        lineaa = lineaa + 1
                        columnaa = 0
                        contadorestructura = 0
                    
                    end if
                    if (contadorestructura == 2) then
                        if (tkn == comentario1Linea) then
                            comentarioabierto = .true.
                            contadorestructura = 0
                            tkn = ""
                            estado = 4
                            puntero = puntero + 1
                            cycle
                        elseif (tkn == comentarioMultilineainicio) then
                            comentarioabierto = .true.
                            contadorestructura = 0
                            tkn = ""
                            estado = 5
                            puntero = puntero + 1
                            cycle
                        end if
                    endif
                    if ((tkn == "Controles") .and. (estructuraabierto .eqv. .true.)) then
                            columnaa = columnaa - 1
                        call agregarlexema(numlexema, tkn, "Fin de controles", lineaa, columnaa)
                            !!print *, "tkn: ", tkn
                            tkn = ""
                            contadorestructura = 0
                            controlesabierto = .false.
                    elseif ((tkn == finestructura) .and. (controlesabierto .eqv. .false.)) then
                            columnaa = columnaa - 1
                            estructuraabierto = .false.
                            call agregarlexema(numlexema, tkn, "Fin de estructura", lineaa, columnaa)
                            !!print *, "tkn: ", tkn
                            tkn = ""
                            contadorestructura = 0

                            estado = 0
                        


                    elseif (char == ' ') then
                        if (any(tkn == objetos)) then
                            columnaa = columnaa - 1
                            if (tkn == objetos(1)) then
                                call agregarlexema(numlexema, tkn, "Etiqueta", lineaa, columnaa)
                                !!print *, "tkn: ", tkn
                                tkn = ""
                                contadorestructura = 0
                            elseif (tkn == objetos(2)) then
                                call agregarlexema(numlexema, tkn, "Boton", lineaa, columnaa)
                                !!print *, "tkn: ", tkn
                                tkn = ""
                                contadorestructura = 0
                            elseif (tkn == objetos(3)) then
                                call agregarlexema(numlexema, tkn, "Check", lineaa, columnaa)
                                !!print *, "tkn: ", tkn
                                tkn = ""
                                contadorestructura = 0
                            elseif (tkn == objetos(4)) then
                                call agregarlexema(numlexema, tkn, "RadioBoton", lineaa, columnaa)
                                !!print *, "tkn: ", tkn
                                tkn = ""
                                contadorestructura = 0
                            elseif (tkn == objetos(5)) then
                                call agregarlexema(numlexema, tkn, "Texto", lineaa, columnaa)
                                !!print *, "tkn: ", tkn
                                tkn = ""
                                contadorestructura = 0
                            elseif (tkn == objetos(6)) then
                                call agregarlexema(numlexema, tkn, "AreaTexto", lineaa, columnaa)
                                !!print *, "tkn: ", tkn
                                tkn = ""
                                contadorestructura = 0
                            elseif (tkn == objetos(7)) then
                                call agregarlexema(numlexema, tkn, "Clave", lineaa, columnaa)
                                !!print *, "tkn: ", tkn
                                tkn = ""
                                contadorestructura = 0
                            elseif (tkn == objetos(8)) then
                                call agregarlexema(numlexema, tkn, "Contenedor", lineaa, columnaa)
                                !!print *, "tkn: ", tkn
                                tkn = ""
                                contadorestructura = 0
                            else
                                call agregarerror(numerrores, lineaa, columnaa, trim(tkn), "Objeto")
                                print *, "error"
                            end if
                        elseif(any(tkn == propiedades)) then
                            columnaa = columnaa - 1
                            if (tkn == propiedades(1)) then
                                call agregarlexema(numlexema, tkn, "setColorLetra", lineaa, columnaa)
                                !!print *, "tkn: ", tkn
                                tkn = ""
                                contadorestructura = 0  
                            elseif (tkn == propiedades(2)) then
                                call agregarlexema(numlexema, tkn, "setTexto", lineaa, columnaa)
                                !!print *, "tkn: ", tkn
                                tkn = ""
                                contadorestructura = 0
                            elseif (tkn == propiedades(3)) then
                                call agregarlexema(numlexema, tkn, "setAlineacion", lineaa, columnaa)
                                !!print *, "tkn: ", tkn
                                tkn = ""
                                contadorestructura = 0
                            elseif (tkn == propiedades(4)) then
                                call agregarlexema(numlexema, tkn, "setColorFondo", lineaa, columnaa)
                                !!print *, "tkn: ", tkn
                                tkn = ""
                                contadorestructura = 0
                            elseif (tkn == propiedades(5)) then
                                call agregarlexema(numlexema, tkn, "setMarcada", lineaa, columnaa)
                                !!print *, "tkn: ", tkn
                                tkn = ""
                                contadorestructura = 0
                            elseif (tkn == propiedades(6)) then
                                call agregarlexema(numlexema, tkn, "setGrupo", lineaa, columnaa)
                                !!print *, "tkn: ", tkn
                                tkn = ""
                                contadorestructura = 0
                            elseif (tkn == propiedades(7)) then
                                call agregarlexema(numlexema, tkn, "setAncho", lineaa, columnaa)
                                !!print *, "tkn: ", tkn
                                tkn = ""
                                contadorestructura = 0
                            elseif (tkn == propiedades(8)) then
                                call agregarlexema(numlexema, tkn, "setAlto", lineaa, columnaa)
                                !!print *, "tkn: ", tkn
                                tkn = ""
                                contadorestructura = 0
                            else
                                call agregarerror(numerrores, lineaa, columnaa, trim(tkn), "Propiedad")
                                print *, "error"

                            endif
                        elseif(any(tkn == colocacion)) then
                            columnaa = columnaa - 1
                            if (tkn == colocacion(1)) then
                                call agregarlexema(numlexema, tkn, "setPosicion", lineaa, columnaa)
                                !!print *, "tkn: ", tkn
                                tkn = ""
                                contadorestructura = 0
                            elseif (tkn == colocacion(2)) then
                                call agregarlexema(numlexema, tkn, "add", lineaa, columnaa)
                                !!print *, "tkn: ", tkn
                                tkn = ""
                                contadorestructura = 0
                            else
                                call agregarerror(numerrores, lineaa, columnaa, trim(tkn), "Colocacion")
                                print *, "error"
                                tkn = ""
                            end if
                        elseif (tkn == "") then
                            columnaa = columnaa + 1
                            contadorestructura = 0
                            puntero = puntero + 1
                            cycle
                        else
                            
                            call agregarerror(numerrores, lineaa, columnaa, trim(tkn), "Control, Propiedad o Colocacion")
                            print *, "error"
                            
                        endif
                    
                    tkn = ""
                    elseif (char == ';') then
                        columnaa = columnaa - 1
                        call agregarlexema(numlexema, tkn, "ID", lineaa, columnaa)
                        !!print *, "tkn: ", tkn
                        tkn = ""
                        contadorestructura = 0
                        call agregarlexema(numlexema, char, "Fin de Control", lineaa, columnaa)
                        !!print *, "tkn: ", char




                    end if







                    
                    
                
                case (2)
                    !estado para cuando esté activada la estructura de propiedades
                    
                    if ((char /= ' ') .and. (char /= ';') .and. (ichar(char) /= 10) .and. (char /= '(') .and. (char /= ')') .and. &
                    ( char /= ',') .and. (char /= '.'))  then
                        tkn = trim(tkn) // char
                        columnaa = columnaa + 1
                        contadorestructura = contadorestructura + 1


                        if ((tkn == 'propiedades') .and. (estructuraabierto .eqv. .true.)) then
                            call agregarlexema(numlexema, tkn, "Fin de Propiedades", lineaa, columnaa)
                            !!print *, "tkn: ", tkn
                            tkn = ""
                            contadorestructura = 0
                            propiedadesabierto = .false.
                        elseif ((tkn == finestructura) .and. (propiedadesabierto .eqv. .false.)) then
                            estructuraabierto = .false.
                            call agregarlexema(numlexema, tkn, "Fin de estructura", lineaa, columnaa)
                            !!print *, "tkn: ", tkn
                            tkn = ""
                            contadorestructura = 0
                            estado = 0
                        end if




                    elseif (char == '.') then
                        columnaa = columnaa - 1
                        if (tkn /= "") then
                            call agregarlexema(numlexema, tkn, "ID", lineaa, columnaa)
                            !!print *, "tkn: ", tkn
                        endif
                            tkn = ""
                        columnaa = columnaa + 1
                        call agregarlexema(numlexema, char, "Punto", lineaa, columnaa)
                        !!print *, "tkn: ", char
                        contadorestructura = 0
                    elseif (char == '(') then
                        if (any(tkn == propiedades)) then
                            call agregarlexema(numlexema, tkn, "Propiedad", lineaa, columnaa)
                            !!print *, "tkn: ", tkn
                            tkn = ""
                            contadorestructura = 0
                        elseif (any(tkn == objetos)) then
                            call agregarlexema(numlexema, tkn, "Objeto", lineaa, columnaa)
                            !!print *, "tkn: ", tkn
                            tkn = ""
                            contadorestructura = 0
                        elseif (any(tkn == colocacion)) then
                            call agregarlexema(numlexema, tkn, "PropiedadC", lineaa, columnaa)
                            !!print *, "tkn: ", tkn
                            tkn = ""
                            contadorestructura = 0
                        else
                            call agregarerror(numerrores, lineaa, columnaa, trim(tkn), "Propiedad")
                            print *, "error"
                            tkn = ""
                            contadorestructura = 0
                        end if
                        call agregarlexema(numlexema, char, "Parentesis Abierto", lineaa, columnaa)
                        !!print *, "tkn: ", char
                        contadorestructura = 0
                    elseif (char == ',') then
                        call agregarlexema(numlexema, tkn, "Valor", lineaa, columnaa)
                        !!print *, "tkn: ", tkn
                        tkn = ""
                        call agregarlexema(numlexema, char, "Coma", lineaa, columnaa)
                        !!print *, "tkn: ", char
                        contadorestructura = 0

                    elseif (char == ')') then
                        if (tkn /= "") then
                        call agregarlexema(numlexema, tkn, "Valor", lineaa, columnaa)
                        !!print *, "tkn: ", tkn
                        tkn = ""
                        endif
                        call agregarlexema(numlexema, char, "Parentesis Cerrado", lineaa, columnaa)
                        !!print *, "tkn: ", char
                        contadorestructura = 0
                    elseif (ichar(char) == 10) then
                        
                        if (tkn /= "") then
                            call agregarerror(numerrores, lineaa, columnaa, trim(tkn), "ID")
                            print *, "error"
                            tkn = ""
                        endif
                        lineaa = lineaa + 1
                        columnaa = 0
                        
                        contadorestructura = 0


                    elseif (char == ';') then
                        call agregarlexema(numlexema, char, "Fin de Propiedad", lineaa, columnaa)
                        !!print *, "tkn: ", char
                        tkn = ""
                        contadorestructura = 0
                    

                    


                    endif


                    
                    if (contadorestructura == 2) then
                        if (tkn == comentario1Linea) then
                            comentarioabierto = .true.
                            contadorestructura = 0
                            tkn = ""
                            estado = 4
                        elseif (tkn == comentarioMultilineainicio) then
                            comentarioabierto = .true.
                            contadorestructura = 0
                            tkn = ""
                            estado = 5
                        end if
                    contadorestructura = 0
                    
                    endif

                
                case (3)
                    !estado para cuando esté activada la estructura de colocacion
                    
                    if ((char /= ' ') .and. (char /= ';') .and. (ichar(char) /= 10) .and. (char /= '(') .and. (char /= ')') .and. &
                    ( char /= ',') .and. (char /= '.'))  then
                        tkn = trim(tkn) // char
                        columnaa = columnaa + 1
                        contadorestructura = contadorestructura + 1


                        if ((tkn == 'Colocacion') .and. (estructuraabierto .eqv. .true.)) then
                            call agregarlexema(numlexema, tkn, "Fin de Colocacion", lineaa, columnaa)
                            !!print *, "tkn: ", tkn
                            tkn = ""
                            contadorestructura = 0
                            propiedadesabierto = .false.
                        elseif ((tkn == finestructura) .and. (propiedadesabierto .eqv. .false.)) then
                            estructuraabierto = .false.
                            call agregarlexema(numlexema, tkn, "Fin de estructura", lineaa, columnaa)
                            !!print *, "tkn: ", tkn
                            tkn = ""
                            contadorestructura = 0
                            estado = 0
                        end if




                    elseif (char == '.') then
                        columnaa = columnaa - 1
                        if (tkn /= "") then
                            call agregarlexema(numlexema, tkn, "ID", lineaa, columnaa)
                            !!print *, "tkn: ", tkn
                        endif
                            tkn = ""
                        columnaa = columnaa + 1
                        call agregarlexema(numlexema, char, "Punto", lineaa, columnaa)
                        !!print *, "tkn: ", char
                        contadorestructura = 0
                    elseif (char == '(') then
                        if (any(tkn == colocacion)) then
                            call agregarlexema(numlexema, tkn, "PropiedadC", lineaa, columnaa)
                            !!print *, "tkn: ", tkn
                            tkn = ""
                            contadorestructura = 0
                        elseif (any(tkn == objetos)) then
                            call agregarlexema(numlexema, tkn, "Objeto", lineaa, columnaa)
                            !!print *, "tkn: ", tkn
                            tkn = ""
                            contadorestructura = 0
                        elseif (any(tkn == propiedades)) then
                            call agregarlexema(numlexema, tkn, "Propiedad", lineaa, columnaa)
                            !!print *, "tkn: ", tkn
                            tkn = ""
                            contadorestructura = 0
                        else
                            call agregarerror(numerrores, lineaa, columnaa, trim(tkn), "PropiedadC")
                            print *, "error"
                            tkn = ""
                            contadorestructura = 0
                        end if
                        call agregarlexema(numlexema, char, "Parentesis Abierto", lineaa, columnaa)
                        !!print *, "tkn: ", char
                        contadorestructura = 0
                    elseif (char == ',') then
                        call agregarlexema(numlexema, tkn, "Valor", lineaa, columnaa)
                        !!print *, "tkn: ", tkn
                        tkn = ""
                        call agregarlexema(numlexema, char, "Coma", lineaa, columnaa)
                        !!print *, "tkn: ", char
                        contadorestructura = 0
                    elseif (char == ')') then
                        if (tkn /= "") then
                            call agregarlexema(numlexema, tkn, "Valor", lineaa, columnaa)
                            !!print *, "tkn: ", tkn
                            tkn = ""
                        end if
                        call agregarlexema(numlexema, char, "Parentesis Cerrado", lineaa, columnaa)
                        !!print *, "tkn: ", char
                        contadorestructura = 0
                    elseif (ichar(char) == 10) then
                        if (tkn /= "") then
                            call agregarerror(numerrores, lineaa, columnaa, trim(tkn), "ID")
                            print *, "error"
                            tkn = ""
                        end if
                        lineaa = lineaa + 1
                        columnaa = 0
                        
                        contadorestructura = 0
                    elseif (char == ';') then
                        call agregarlexema(numlexema, char, "Fin de Propiedad", lineaa, columnaa)
                        !!print *, "tkn: ", char
                        tkn = ""
                        contadorestructura = 0
                    

                    


                    endif


                    
                    if (contadorestructura == 2) then
                        if (tkn == comentario1Linea) then
                            comentarioabierto = .true.
                            contadorestructura = 0
                            tkn = ""
                            estado = 4
                        elseif (tkn == comentarioMultilineainicio) then
                            comentarioabierto = .true.
                            contadorestructura = 0
                            tkn = ""
                            estado = 5
                        end if
                    contadorestructura = 0
                    
                    endif

                

                    
                    
                
                case (4)
                    !estado para comentarios de una linea
                    if (ichar(char) == 10) then
                        columnaa = 0
                        lineaa = lineaa + 1
                        comentarioabierto = .false.

                        if (controlesabierto .eqv. .true.) then
                            estado = 1
                        elseif (propiedadesabierto .eqv. .true.) then
                            estado = 2
                        elseif (colocacionabierto .eqv. .true.) then
                            estado = 3
                        else
                            estado = 0
                        end if
                    end if
                case (5)
                    !estado para comentarios multilinea
                    if (char == '*') then
                        tkn = trim(tkn) // char
                        contadorestructura = contadorestructura + 1
                        
                    elseif (tkn == '*') then
                        tkn = trim(tkn) // char
                        contadorestructura = contadorestructura + 1
                    

                    elseif (contadorestructura == 2) then
                            if (tkn == comentarioMultilineafin) then
                                comentarioabierto = .false.
                                contadorestructura = 0
                                tkn = ""
                                if (controlesabierto .eqv. .true.) then
                                    estado = 1
                                elseif (propiedadesabierto .eqv. .true.) then
                                    estado = 2
                                elseif (colocacionabierto .eqv. .true.) then
                                    estado = 3
                                else
                                    estado = 0
                                end if
                            end if
                    elseif (ichar(char) == 10) then
                        lineaa = lineaa + 1
                    else
                        contadorestructura = 0
                    end if
                
                    
                

                        

                                        
                
                end select
                puntero = puntero + 1
                
                
            end if
        end do

        errorest = 'C:\Cursos\Fortran\Lab_LFP\_LFP_2S24Proyectos_202300645\Proyecto2\errores.txt'
        open(unit=1, file=errorest, status='replace', action='write', iostat=ios)



        if(numErrores > 0) then
            errorest = 'C:\Cursos\Fortran\Lab_LFP\_LFP_2S24Proyectos_202300645\Proyecto2\errores.txt'
            open(unit=1, file=errorest, status='unknown', action='write', iostat=ios, position='append')

            print *, "Errores encontrados: ", numErrores
            do i=1,numErrores
                print *, "No. ", listae(i)%numeroe, " // Linea: ", listae(i)%lineae, " // Columna: ", listae(i)%columnae, " // ",&
                listae(i)%errore, " // ", listae(i)%tkn_esperado
      
                
                write(1, '(I0)') listae(i)%lineae
                write(1, '(I0)') listae(i)%columnae
                write(1, '(A)') listae(i)%errore
                write(1, '(A)') listae(i)%tkn_esperado


            end do





            
        end if
        
        do i=1,numlexema
        
            write(*, '(A, I0, A, A, A, A, A,I0, A,I0,A)') "No.", listal(i)%numerol, " // Lex:  ", trim(listal(i)%lexemal), " // ",&
            listal(i)%tipol," // lc(", listal(i)%lineal, ",", listal(i)%columnal , ")"
            print *, ""
        end do


        linkhtml = 'C:\Cursos\Fortran\Lab_LFP\_LFP_2S24Proyectos_202300645\Proyecto2\lexemas.html'
        open(unit=2, file=linkhtml, status='replace', action='write', iostat=ios)

        
        write(2,*) '<!DOCTYPE html>' // achar(10), &
'<html lang="es">' // achar(10), &
'<head>' // achar(10), &
'    <meta charset="UTF-8">' // achar(10), &
'    <meta name="viewport" content="width=device-width, initial-scale=1.0">' // achar(10), &
'    <title>Tabla de Errores</title>' // achar(10), &
'    <style>' // achar(10), &
'        table {' // achar(10), &
'            width: 100%;' // achar(10), &
'            border-collapse: collapse;' // achar(10), &
'        }' // achar(10), &
'        th, td {' // achar(10), &
'            border: 1px solid black;' // achar(10), &
'            padding: 8px;' // achar(10), &
'            text-align: left;' // achar(10), &
'        }' // achar(10), &
'        th {' // achar(10), &
'            background-color: #f2f2f2;' // achar(10), &
'        }' // achar(10), &
'    </style>' // achar(10), &
'</head>' // achar(10), &
'<body>' // achar(10), &
'    <h1>Tabla de Tokens</h1>' // achar(10), &
'    <table>' // achar(10), &
'        <thead>' // achar(10), &
'            <tr>' // achar(10), &
'                <th>No</th>' // achar(10), &
'                <th>Lexema</th>' // achar(10), &
'                <th>Tipo</th>' // achar(10), &
'                <th>Fila</th>' // achar(10), &
'                <th>Columna</th>' // achar(10), &
'            </tr>' // achar(10), &
'        </thead>' // achar(10), &
'        <tbody>' // achar(10)
close(2)


do i = 1, numlexema
        open(unit=2, file=linkhtml, status='old', action='write', iostat=ios, position='append')
        if (listal(i)%lexemal == "<!--") then
            write(2,*) '            <!-- Aquí puedes agregar filas de datos -->' // achar(10), &
            '            <tr>' // achar(10), &
            '                <td>',i,'</td>' // achar(10), &
            '                <td> < !--</td>' // achar(10), &
            '                <td>',listal(i)%tipol,'</td>' // achar(10), &
            '                <td>',listal(i)%lineal,'</td>' // achar(10), &
            '                <td>',listal(i)%columnal,'</td>' // achar(10), &
            '            </tr>' // achar(10)

        
        
        else
        write(2,*) '            <!-- Aquí puedes agregar filas de datos -->' // achar(10), &
'            <tr>' // achar(10), &
'                <td>',i,'</td>' // achar(10), &
'                <td>',trim(listal(i)%lexemal),'</td>' // achar(10), &
'                <td>',listal(i)%tipol,'</td>' // achar(10), &
'                <td>',listal(i)%lineal,'</td>' // achar(10), &
'                <td>',listal(i)%columnal,'</td>' // achar(10), &
'            </tr>' // achar(10)
        endif
close(2)
    


end do

         
        print *, "/////////////////////////////////////////////////////////"

        do while (contadorlexico < numlexema)
        

            select case (estadolexico)
            case (0)
                contadorlexico = contadorlexico + 1
                if (listal(contadorlexico)%tipol == "Inicio de estructura") then
                    if (listal(contadorlexico + 1)%tipol == "Inicio de controles") then
                        print *, "Inicio de estructura"
                        print *, "Inicio de controles"
                        estadolexico = 1
                        contadorlexico = contadorlexico + 2
                    elseif (listal(contadorlexico + 1)%tipol == "Inicio de propiedades") then
                        print *, "Inicio de estructura"
                        print *, "Inicio de propiedades"
                        estadolexico = 2
                        contadorlexico = contadorlexico + 2
                    elseif ( listal(contadorlexico + 1)%tipol == "Inicio de colocacion") then
                        print *, "Inicio de estructura"
                        print *, "Inicio de colocacion"
                        estadolexico = 4
                        contadorlexico = contadorlexico + 2
                    
                    end if

                end if
            case (1)
                
                print *, listal(contadorlexico)%tipol
                if (any(listal(contadorlexico)%lexemal == objetos)) then
                    !print *, "Objeto"
                    
                    contadorlexico = contadorlexico + 1

                    if (listal(contadorlexico)%tipol == "ID") then
                       ! print *, "Nombre Objeto"
                        
                        contadorlexico = contadorlexico + 1

                        if (listal(contadorlexico)%tipol == "Fin de Control") then
                            !print *, "Fin de Control"
                            contadorlexico = contadorlexico + 1
                        
                        else
                            print *, "Error"
                            call agregarerrors(numerroresintactico, listal(contadorlexico)%lineal, listal(contadorlexico)%columnal,& 
                            listal(contadorlexico)%lexemal, "Fin de Control")
                            contadorlexico = contadorlexico + 1

                        
                        endif

                    else
                        print *, "Error"
                        call agregarerrors(numerroresintactico, listal(contadorlexico)%lineal, &
                        listal(contadorlexico)%columnal, listal(contadorlexico)%lexemal, "ID")
                        contadorlexico = contadorlexico + 1
                    endif


                
                elseif(listal(contadorlexico)%tipol == "Fin de Control") then
                    print *, "reinicio de pruebas"
                    contadorlexico = contadorlexico + 1


                elseif (listal(contadorlexico)%tipol == "Fin de controles") then
                    print *, "Fin de controles"
                    contadorlexico = contadorlexico + 1
                    if (listal(contadorlexico)%tipol == "Fin de estructura") then
                        print *, "Fin de estructura"

                        estadolexico = 0

                        
                    else
                        print *, "Error"
                        call agregarerrors(numerroresintactico, listal(contadorlexico)%lineal, listal(contadorlexico)%columnal, & 
                        listal(contadorlexico)%lexemal, "Fin de estructura")
                        contadorlexico = contadorlexico + 1
                    endif
                else

                    print *, "Error"
                    call agregarerrors(numerroresintactico, listal(contadorlexico)%lineal, listal(contadorlexico)%columnal, &
                    listal(contadorlexico)%lexemal, "Objeto")
                    contadorlexico = contadorlexico + 1
                endif
                
            case (2)
                print *, listal(contadorlexico)%tipol, "//estado 2"
                
                if (listal(contadorlexico)%tipol == "ID") then
                    print *, "NombrePropiedad"
                    contadorlexico = contadorlexico + 1
                    if (listal(contadorlexico)%tipol == "Punto") then
                        print *, "Punto"
                        contadorlexico = contadorlexico + 1
                        if (listal(contadorlexico)%tipol == "Propiedad") then
                            print *, "Propiedad"
                            contadorlexico = contadorlexico + 1
                            if (listal(contadorlexico)%tipol == "Parentesis Abierto") then
                                print *, "Parentesis Abierto"
                                contadorlexico = contadorlexico + 1
                                estadolexico = 3
                                
                            else
                                print *, "Error"
                                call agregarerrors(numerroresintactico, listal(contadorlexico)%lineal, &
                                listal(contadorlexico)%columnal, listal(contadorlexico)%lexemal, "Parentesis Abierto")
                                contadorlexico = contadorlexico + 1
                            endif

                        else
                            print *, "Error"
                            call agregarerrors(numerroresintactico, listal(contadorlexico)%lineal, listal(contadorlexico)%columnal,& 
                            listal(contadorlexico)%lexemal, "Propiedad")
                            contadorlexico = contadorlexico + 1
                        endif
                    
                    else
                        print *, "Error"
                        call agregarerrors(numerroresintactico, listal(contadorlexico)%lineal, listal(contadorlexico)%columnal, & 
                        listal(contadorlexico)%lexemal, "Punto")
                        contadorlexico = contadorlexico + 1

                    endif
                elseif (listal(contadorlexico)%tipol == "Fin de Propiedades") then
                    print *, "Fin de propiedades"
                    contadorlexico = contadorlexico + 1
                    if (listal(contadorlexico)%tipol == "Fin de estructura") then
                        print *, "Fin de estructura"
                        estadolexico = 0
                        
                    endif

                
                else
                    print *, "Error2"
                    call agregarerrors(numerroresintactico, listal(contadorlexico)%lineal, listal(contadorlexico)%columnal, &
                    listal(contadorlexico)%lexemal, "NombrePropiedad")
                    contadorlexico = contadorlexico + 1
                endif

            case (3)
                if (listal(contadorlexico)%tipol == "Valor") then
                    print *, "Valor"
                    contadorlexico = contadorlexico + 1
                    if (listal(contadorlexico)%tipol == "Coma") then
                        print *, "Coma"
                        contadorlexico = contadorlexico + 1
                    elseif (listal(contadorlexico)%tipol == "Parentesis Cerrado") then
                        print *, "Parentesis Cerrado"
                        contadorlexico = contadorlexico + 1
                        if (listal(contadorlexico)%tipol == "Fin de Propiedad") then
                            print *, "Fin de Propiedad"
                            contadorlexico = contadorlexico + 1
                            estadolexico = 2
                        else
                            print *, "Error"
                            call agregarerrors(numerroresintactico, listal(contadorlexico)%lineal, listal(contadorlexico)%columnal,& 
                            listal(contadorlexico)%lexemal, "Fin de Propiedad")
                            contadorlexico = contadorlexico + 1
                        endif

                    else 
                        print *, "Error"
                        call agregarerrors(numerroresintactico, listal(contadorlexico)%lineal, listal(contadorlexico)%columnal, &
                        listal(contadorlexico)%lexemal, "Coma o Parentesis Cerrado")
                        contadorlexico = contadorlexico + 1
                    endif
                elseif (listal(contadorlexico)%tipol == "Parentesis Cerrado") then
                    print *, "error"
                    call agregarerrors(numerroresintactico, listal(contadorlexico)%lineal, listal(contadorlexico)%columnal, &
                    listal(contadorlexico)%lexemal, "Uno o mas valores")
                    contadorlexico = contadorlexico + 1

                    if (listal(contadorlexico)%tipol == "Fin de Propiedad") then
                        print *, "Fin de Propiedad"
                        contadorlexico = contadorlexico + 1
                        estadolexico = 2
                    else
                        print *, "Error"
                        call agregarerrors(numerroresintactico, listal(contadorlexico)%lineal, listal(contadorlexico)%columnal,& 
                        listal(contadorlexico)%lexemal, "Fin de Propiedad")
                        contadorlexico = contadorlexico + 1
                    endif
                elseif (listal(contadorlexico)%tipol == "Fin de Propiedad") then
                    print *, "Fin de Propiedad"
                    contadorlexico = contadorlexico + 1
                    estadolexico = 2

                else 
                    print *, "Error"
                    call agregarerrors(numerroresintactico, listal(contadorlexico)%lineal, listal(contadorlexico)%columnal, &
                    listal(contadorlexico)%lexemal, "Punto y coma de finalizacion")
                    contadorlexico = contadorlexico + 1
                endif
            
            case (4)
                print *, listal(contadorlexico)%tipol, "//estado 4"
                
                if (listal(contadorlexico)%tipol == "ID") then
                    print *, "NombrePropiedad"
                    contadorlexico = contadorlexico + 1
                    if (listal(contadorlexico)%tipol == "Punto") then
                        print *, "Punto"
                        contadorlexico = contadorlexico + 1
                        if (listal(contadorlexico)%tipol == "PropiedadC") then
                            print *, "PropiedadC"
                            contadorlexico = contadorlexico + 1
                            if (listal(contadorlexico)%tipol == "Parentesis Abierto") then
                                print *, "Parentesis Abierto"
                                contadorlexico = contadorlexico + 1
                                estadolexico = 5
                                
                            else
                                print *, "Error"
                                call agregarerrors(numerroresintactico, listal(contadorlexico)%lineal, &
                                listal(contadorlexico)%columnal, listal(contadorlexico)%lexemal, "Parentesis Abierto")
                                contadorlexico = contadorlexico + 1
                            endif

                        else
                            print *, "Error"
                            call agregarerrors(numerroresintactico, listal(contadorlexico)%lineal, listal(contadorlexico)%columnal,& 
                            listal(contadorlexico)%lexemal, "Propiedad")
                            contadorlexico = contadorlexico + 1
                        endif
                    
                    else
                        print *, "Error"
                        call agregarerrors(numerroresintactico, listal(contadorlexico)%lineal, listal(contadorlexico)%columnal, & 
                        listal(contadorlexico)%lexemal, "Punto")
                        contadorlexico = contadorlexico + 1

                    endif
                elseif (listal(contadorlexico)%tipol == "Fin de Colocacion") then
                    print *, "Fin de Colocacion"
                    contadorlexico = contadorlexico + 1
                    if (listal(contadorlexico)%tipol == "Fin de estructura") then
                        print *, "Fin de estructura"
                        estadolexico = 0
                        
                    endif

                
                else
                    print *, "Error4"
                    call agregarerrors(numerroresintactico, listal(contadorlexico)%lineal, listal(contadorlexico)%columnal, &
                    listal(contadorlexico)%lexemal, "NombrePropiedadC")
                    contadorlexico = contadorlexico + 1
                endif

            case (5)
                if (listal(contadorlexico)%tipol == "Valor") then
                    print *, "Valor"
                    contadorlexico = contadorlexico + 1
                    if (listal(contadorlexico)%tipol == "Coma") then
                        print *, "Coma"
                        contadorlexico = contadorlexico + 1
                    elseif (listal(contadorlexico)%tipol == "Parentesis Cerrado") then
                        print *, "Parentesis Cerrado"
                        contadorlexico = contadorlexico + 1
                        if (listal(contadorlexico)%tipol == "Fin de Propiedad") then
                            print *, "Fin de Propiedad"
                            contadorlexico = contadorlexico + 1
                            estadolexico = 4
                        else
                            print *, "Error"
                            call agregarerrors(numerroresintactico, listal(contadorlexico)%lineal, listal(contadorlexico)%columnal,& 
                            listal(contadorlexico)%lexemal, "Fin de Propiedad")
                            contadorlexico = contadorlexico + 1
                        endif

                    else 
                        print *, "Error"
                        call agregarerrors(numerroresintactico, listal(contadorlexico)%lineal, listal(contadorlexico)%columnal, &
                        listal(contadorlexico)%lexemal, "Coma o Parentesis Cerrado")
                        contadorlexico = contadorlexico + 1
                    endif
                elseif (listal(contadorlexico)%tipol == "Parentesis Cerrado") then
                    print *, "error"
                    call agregarerrors(numerroresintactico, listal(contadorlexico)%lineal, listal(contadorlexico)%columnal, &
                    listal(contadorlexico)%lexemal, "Uno o mas valores")
                    contadorlexico = contadorlexico + 1

                    if (listal(contadorlexico)%tipol == "Fin de Propiedad") then
                        print *, "Fin de Propiedad"
                        contadorlexico = contadorlexico + 1
                        estadolexico = 4
                    else
                        print *, "Error"
                        call agregarerrors(numerroresintactico, listal(contadorlexico)%lineal, listal(contadorlexico)%columnal,& 
                        listal(contadorlexico)%lexemal, "Fin de Propiedad")
                        contadorlexico = contadorlexico + 1
                    endif
                elseif (listal(contadorlexico)%tipol == "Fin de Propiedad") then
                    print *, "Fin de Propiedad"
                    contadorlexico = contadorlexico + 1
                    estadolexico = 4

                else 
                    print *, "Error"
                    call agregarerrors(numerroresintactico, listal(contadorlexico)%lineal, listal(contadorlexico)%columnal, &
                    listal(contadorlexico)%lexemal, "Punto y coma de finalizacion")
                    contadorlexico = contadorlexico + 1
                endif
            
                
            
            end select
        print *, contadorlexico, "jeje"
        end do
    

    errorest = 'C:\Cursos\Fortran\Lab_LFP\_LFP_2S24Proyectos_202300645\Proyecto2\erroresintacticos.txt'
    open(unit=1, file=errorest, status='replace', action='write', iostat=ios)

    if(numerroresintactico > 0) then
        errorest = 'C:\Cursos\Fortran\Lab_LFP\_LFP_2S24Proyectos_202300645\Proyecto2\erroresintacticos.txt'
        open(unit=1, file=errorest, status='unknown', action='write', iostat=ios, position='append')

        print *, "Errores encontrados: ", numerroresintactico
        do i=1,numerroresintactico
            print *, "No. ", listaesin(i)%numeroe, " // Linea: ", listaesin(i)%lineae, " // Columna: ", listaesin(i)%columnae, &
            " // ", listaesin(i)%errore, " // ", listaesin(i)%tkn_esperado

            write(1, '(I0)') listaesin(i)%lineae
            write(1, '(I0)') listaesin(i)%columnae
            write(1, '(A)') listaesin(i)%errore
            write(1, '(A)') listaesin(i)%tkn_esperado


            

        end do
    endif

    close(1)
    
end program name