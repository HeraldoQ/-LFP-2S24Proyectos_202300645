module funciones
    implicit none

    Type :: clerrores
    integer :: numeroe
    character :: errore
    character :: descripcione
    integer :: columnae
    integer :: lineae
    end Type clerrores

    type(clerrores), allocatable :: listae(:)

    Type :: lexema
    integer :: numerol
    !si se trunca algo se hace desde aqui
    character(len = 29) :: lexemal
    character(len=20) :: tipol
    integer :: columnal
    integer :: lineal
    end Type lexema

    type(lexema), allocatable :: listal(:)

    Type :: continente
        character :: continente
        character :: pais
        character :: poblacion
        integer :: saturacion
        character :: bandera
    end Type continente

    type(continente), allocatable :: listac(:)



    contains

    subroutine cerrores(st,numeroe, errore, descripcione, lineae, columnae)
        type(clerrores), intent(out) :: st
        integer :: numeroe
        character  :: errore
        character(len = *) :: descripcione
        integer :: columnae
        integer  :: lineae

        st%numeroe = numeroe
        st%errore = errore
        st%descripcione = descripcione
        st%columnae = columnae
        st%lineae = lineae

    


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

    ! subroutine clexema(st,numerol, errorl, descripcionl, columnal, lineal)
    !     type(lexema), intent(out) :: st
    !     integer :: numerol
    !     character  :: errorl
    !     character :: descripcionl
    !     integer :: columnal
    !     integer  :: lineal

    !     st%numerol = numerol
    !     st%errorl = errorl
    !     st%descripcionl = descripcionl
    !     st%columnal = columnal
    !     st%lineal = lineal

    ! end subroutine clexema


    subroutine ccontinente(st, continentec, paisc, poblacionc, saturacionc, banderac)
        type(continente), intent(out) :: st
        character :: continentec
        character :: paisc
        character :: poblacionc
        integer :: saturacionc
        character :: banderac

        st%continente = continentec
        st%pais = paisc
        st%poblacion = poblacionc
        st%saturacion = saturacionc
        st%bandera = banderac

    end subroutine ccontinente








end module funciones


program analizador
    use funciones
    implicit none
    character(len=200) :: linea
    character(len=20000) :: entrada
    integer :: ios


                                                                                !contador para s de entrada
    integer :: i, len, lineaa, columnaa,estado, puntero, numErrores, file_unit, tkn_pais, contador,contadorc, contadorsat, &
    contadorpaises
    integer :: espacio_texto
    character(len=10000) :: cadena, buffer
    !character(len=10000) :: entrada
    character(len=1) :: char
                                        !palabra es para que lo agarre como palabra, sino lo trunca no se porque
    character(len=30) :: tkn
    character(len=200) :: tknaper,palabra

    !para todo lo concerniente a tokens y eso
    character(len=1), dimension(26) :: mayus, minus
    character(len=1), dimension(4) :: ayc
    character(len=1) :: comi
    character(len=1), dimension(10) :: num
    !char error no es un token, ni errores
    character(len=1) :: char_error, cierre
    integer, dimension(100,4) :: errores
    character(len = 15), dimension(7) :: pr
    logical :: comillas = .false.
    
    
    !lo concerniente a las listas que harán lo de python
    character(len=100) :: continentef
    
    
    
    !instancias de lexema
    type(lexema) :: lex
    type(clerrores) :: err

    !contador de numero de lexemas o lo que sea
    integer :: numlex


    

    allocate(listae(100))
    allocate(listal(100))

    

    !tokens admitidos
    mayus = [ 'A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'I', 'J', 'K', 'L', 'M', 'N', 'O', 'P', 'Q', 'R', &
    'S', 'T', 'U', 'V', 'W', 'X', 'Y', 'Z' ]
    
    minus = [ 'a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k', 'l', 'm', 'n', 'o', 'p', 'q', 'r', &
    's', 't', 'u', 'v', 'w', 'x', 'y', 'z' ]

    ayc = [ ':', '{', '}' , ';' ]

    comi = '"' 

    num = [ '0', '1', '2', '3', '4', '5', '6', '7', '8', '9' ]



    

    


    pr = [ 'grafica   ', 'continente', 'nombre    ', 'pais      ', 'poblacion ', 'saturacion', 'bandera   ' ]    




    
    estado = 0
    puntero = 1
    columnaa = 0
    lineaa = 1
    numErrores = 0

    
    contador = 0
    contadorc = 0
    numlex = 0
    contadorsat = 0
    contadorpaises = 0
    



    call system ("cls")
    call system ("cls")
    entrada = ""
    tkn = " "

    do
        
        read(*,'(A)', iostat=ios) linea
        if (ios /= 0) exit
        if (trim(linea) == "") exit
        if (entrada /= "") then
            
            entrada = trim(entrada) // new_line('a') // trim(linea)
        
        
        else
            entrada = trim(linea)
        end if 
        
    end do

    len = len_trim(entrada)
    print *, "entrada: ", entrada




    !/////////////////////////analizador lexico///////////////////////////
    do while (puntero <=len)
    
    char = entrada(puntero:puntero)
    print *, trim(char)

    if (ichar(char) == 10) then
        lineaa = lineaa + 1
        columnaa = 0
        puntero = puntero + 1
        print *, "salto de lineaz"
    elseif (ichar(char) == 9) then
        columnaa = columnaa + 1
        puntero = puntero + 1
        print *, "tabulador"
    elseif (ichar(char) == 32 .and. (comillas .eqv. .false.)) then
        columnaa = columnaa + 1
        puntero = puntero + 1
        print *, "espacio"
    elseif (ichar(char) == 00 .or. ichar(char) == 11) then
        columnaa = columnaa + 1
        puntero = puntero + 1
        print *, "caracter nulo"
    else
        
        select case (estado)
        !case 0 para los que sean palabras reservadas
        case (0)
            
            if (any(char == minus)) then
                !!print *, "Coincidencia en columna ", columnaa, " linea ", lineaa
                !!print *, "Antes de concatenar: tkn = '", trim(tkn), "' char = '", trim(char), "'"
                
                tkn = trim(tkn) // char
                !!print *, "Después de concatenar: tkn = '", trim(tkn), "'"
                columnaa = columnaa + 1
                !!print *, "<<<<",trim(tkn)
                !!print *, ">>>>",trim(char)
            elseif (any(char == ayc) .and. contador <2) then
                tknaper = char
                contador = contador + 1
                columnaa = columnaa + 1
                !añade el lexema a la lista de lexemas
                numlex = numlex + 1
                palabra = "apertura"
                listal(numlex)%numerol = numlex
                listal(numlex)%lexemal = trim(tknaper)
                listal(numlex)%tipol = trim(palabra)
                listal(numlex)%columnal = columnaa
                listal(numlex)%lineal = lineaa
                
                tknaper = ""
                tkn = ""
            elseif (any(char == num)) then
                tkn = trim(tkn) // char
                columnaa = columnaa + 1
            
                
            else
                numErrores = numErrores + 1
                errores(numErrores,:) = (/ ichar(char), 69, columnaa, lineaa/)
                columnaa = columnaa + 1
                print *, "error"
            end if

            if (contador == 2) then
                
                estado = 1
                contador = 0
                print *, "contador reseteado"
            end if
            if (any(tkn == pr)) then
                numlex = numlex + 1
                palabra = "palabra reservada"
                listal(numlex)%numerol = numlex
                listal(numlex)%lexemal = trim(tkn)
                listal(numlex)%tipol = trim(palabra)
                listal(numlex)%columnal = columnaa
                listal(numlex)%lineal = lineaa
                tkn = ""
            
            end if
            
            
            
            
            

        !case 2 es para nombres 
        case (1)
            !se concatena si pertenece a los tokens de letras minusculas, mientras que no haya una apertura de comillas, pues ahi 
            !se debe saltar las reglas de los tokens

            if ((any(char == minus) .and. (comillas .eqv. .false.)) .or. char == " ") then
                !!print *, "Coincidencia en columna ", columnaa, " linea ", lineaa
                !!print *, "Antes de concatenar: tkn = '", trim(tkn), "' char = '", trim(char), "'"
                if (char == " ") then
                    tkn = trim(tkn) // "-"
                else
                    tkn = trim(tkn) // char
                    columnaa = columnaa + 1
                end if
                
                !!print *, "Después de concatenar: tkn = '", trim(tkn), "'"
                
                !!print *, "<<<<",trim(tkn)
                !!print *, ">>>>",trim(char)

            !si no se cumple lo de arriba, prueba si es un token de apertura, si es asi, se añade a la lista de lexemas directo
            elseif (any(char == ayc) .and. contador <2) then
                tknaper = char
                contador = contador + 1
                columnaa = columnaa + 1
                !añade el lexema a la lista de lexemas
                numlex = numlex + 1
                palabra = "apertura"
                listal(numlex)%numerol = numlex
                listal(numlex)%lexemal = trim(tknaper)
                listal(numlex)%tipol = trim(palabra)
                listal(numlex)%columnal = columnaa
                listal(numlex)%lineal = lineaa
                
                tknaper = ""
                tkn = ""
            !verifica si el caracter actual es una comilla, si es asi, se activa el flag de comillas, y tambien se añade la 
            !comilla a la lista de lexemas
            elseif (char == comi .and. contadorc <2) then
                contadorc = contadorc + 1
                comillas = .true.
                print *, "comilla o fin de comilla"
                !columnaa = columnaa + 1
                tknaper = char
                char = ""
                numlex = numlex + 1
                palabra = "comillas"
                listal(numlex)%numerol = numlex
                listal(numlex)%lexemal = trim(tknaper)
                listal(numlex)%tipol = trim(palabra)
                listal(numlex)%columnal = columnaa
                listal(numlex)%lineal = lineaa
                tknaper = ""
                
            else
                !si no se cumple ninguna de las condiciones anteriores, se añade a la lista de errores
                if (comillas .eqv. .false.) then
                numErrores = numErrores + 1
                errores(numErrores,:) = (/ ichar(char), 69, columnaa, lineaa/)
                columnaa = columnaa + 1
                print *, "error1"
                end if
            end if

            !si se cumplen 2 tokens de apertura o cierre, se cambia de estado
            if (contador == 2) then
                
                estado = 2
                contador = 0
            end if
            !si hay 2 comillas, se añade a la lista de lexemas la variable tkn
            if (contadorc == 2 .and. tkn .ne. "") then

                numlex = numlex + 1

                palabra = "cadena"
                listal(numlex)%numerol = numlex
                listal(numlex)%lexemal = trim(tkn)
                listal(numlex)%tipol = trim(palabra)
                listal(numlex)%columnal = columnaa
                listal(numlex)%lineal = lineaa
                !!print *, "token: ", tkn
                tkn = ""


                columnaa = columnaa + 1
                comillas = .false.
                contadorc = 0

                
            end if


            if (comillas .eqv. .true. .and. char .ne. comi) then
                
                tkn = trim(tkn) // char
                columnaa = columnaa + 1
            end if

            if (any(tkn == pr)) then
                numlex = numlex + 1
                palabra = "palabra reservada"
                listal(numlex)%numerol = numlex
                listal(numlex)%lexemal = trim(tkn)
                listal(numlex)%tipol = trim(palabra)
                listal(numlex)%columnal = columnaa
                listal(numlex)%lineal = lineaa
                tkn = ""
            

            end if
        !case 3 es otra vez para definicion de palabras reservadas
        case (2)
            if (any(char == minus)) then
                !!print *, "Coincidencia en columna ", columnaa, " linea ", lineaa
                !!print *, "Antes de concatenar: tkn = '", trim(tkn), "' char = '", trim(char), "'"
                
                tkn = trim(tkn) // char
                !!print *, "Después de concatenar: tkn = '", trim(tkn), "'"
                columnaa = columnaa + 1
                !!print *, "<<<<",trim(tkn)
                !!print *, ">>>>",trim(char)
            elseif (any(char == ayc) .and. contador <2) then
                tknaper = char
                contador = contador + 1
                columnaa = columnaa + 1
                !añade el lexema a la lista de lexemas
                numlex = numlex + 1
                palabra = "apertura"
                listal(numlex)%numerol = numlex
                listal(numlex)%lexemal = trim(tknaper)
                listal(numlex)%tipol = trim(palabra)
                listal(numlex)%columnal = columnaa
                listal(numlex)%lineal = lineaa
                
                tknaper = ""
                tkn = ""
            elseif (any(char == num)) then
                tkn = trim(tkn) // char
                columnaa = columnaa + 1
            
                
            else
                numErrores = numErrores + 1
                errores(numErrores,:) = (/ ichar(char), 69, columnaa, lineaa/)
                columnaa = columnaa + 1
                print *, "error"
            end if

            if (contador == 2) then
                
                estado = 3
                contador = 0
                !!print *, "contador reseteado"
            end if
            if (any(tkn == pr)) then
                numlex = numlex + 1
                palabra = "palabra reservada"
                listal(numlex)%numerol = numlex
                listal(numlex)%lexemal = trim(tkn)
                listal(numlex)%tipol = trim(palabra)
                listal(numlex)%columnal = columnaa
                listal(numlex)%lineal = lineaa
                tkn = ""
            
            end if
            
        !el case 3 es para nombres de continentes y ahora si vienen los paises
        case(3)
            !se concatena si pertenece a los tokens de letras minusculas, mientras que no haya una apertura de comillas, pues ahi 
            !se debe saltar las reglas de los tokens

            if ((any(char == minus) .and. (comillas .eqv. .false.)) .or. char == " ") then
                !!print *, "Coincidencia en columna ", columnaa, " linea ", lineaa
                !!print *, "Antes de concatenar: tkn = '", trim(tkn), "' char = '", trim(char), "'"
                if (char == " ") then
                    tkn = trim(tkn) // "-"
                else
                    tkn = trim(tkn) // char
                    columnaa = columnaa + 1
                end if
                
                !!print *, "Después de concatenar: tkn = '", trim(tkn), "'"
                
                !!print *, "<<<<",trim(tkn)
                !!print *, ">>>>",trim(char)

            !si no se cumple lo de arriba, prueba si es un token de apertura, si es asi, se añade a la lista de lexemas directo
            elseif (any(char == ayc) .and. contador <2) then
                tknaper = char
                contador = contador + 1
                columnaa = columnaa + 1
                !añade el lexema a la lista de lexemas
                numlex = numlex + 1
                palabra = "apertura"
                listal(numlex)%numerol = numlex
                listal(numlex)%lexemal = trim(tknaper)
                listal(numlex)%tipol = trim(palabra)
                listal(numlex)%columnal = columnaa
                listal(numlex)%lineal = lineaa
                
                tknaper = ""
                tkn = ""
            !verifica si el caracter actual es una comilla, si es asi, se activa el flag de comillas, y tambien se añade la 
            !comilla a la lista de lexemas
            elseif (char == comi .and. contadorc <2) then
                contadorc = contadorc + 1
                comillas = .true.
                print *, "comilla o fin de comilla"
                !columnaa = columnaa + 1
                tknaper = char
                char = ""
                numlex = numlex + 1
                palabra = "comillas"
                listal(numlex)%numerol = numlex
                listal(numlex)%lexemal = trim(tknaper)
                listal(numlex)%tipol = trim(palabra)
                listal(numlex)%columnal = columnaa
                listal(numlex)%lineal = lineaa
                tknaper = ""
                
            else
                !si no se cumple ninguna de las condiciones anteriores, se añade a la lista de errores
                if (comillas .eqv. .false.) then
                numErrores = numErrores + 1
                errores(numErrores,:) = (/ ichar(char), 69, columnaa, lineaa/)
                columnaa = columnaa + 1
                print *, "error1"
                end if
            end if

            !si se cumplen 2 tokens de apertura o cierre, se cambia de estado
            if (contador == 2) then
                
                estado = 4
                contador = 0
            end if
            !si hay 2 comillas, se añade a la lista de lexemas la variable tkn
            if (contadorc == 2 .and. tkn .ne. "") then

                numlex = numlex + 1

                palabra = "cadena"
                listal(numlex)%numerol = numlex
                listal(numlex)%lexemal = trim(tkn)
                listal(numlex)%tipol = trim(palabra)
                listal(numlex)%columnal = columnaa
                listal(numlex)%lineal = lineaa
                !!print *, "token: ", tkn
                tkn = ""


                columnaa = columnaa + 1
                comillas = .false.
                contadorc = 0

                
            end if


            if (comillas .eqv. .true. .and. char .ne. comi) then
                
                tkn = trim(tkn) // char
                columnaa = columnaa + 1
            end if

            if (any(tkn == pr)) then
                numlex = numlex + 1
                palabra = "palabra reservada"
                listal(numlex)%numerol = numlex
                listal(numlex)%lexemal = trim(tkn)
                listal(numlex)%tipol = trim(palabra)
                listal(numlex)%columnal = columnaa
                listal(numlex)%lineal = lineaa
                tkn = ""
            

            end if
            
        !el case 4 define la palabra reservada de pais
        case(4)
            if (any(char == minus)) then
                !!print *, "Coincidencia en columna ", columnaa, " linea ", lineaa
                !!print *, "Antes de concatenar: tkn = '", trim(tkn), "' char = '", trim(char), "'"
                
                tkn = trim(tkn) // char
                !!print *, "Después de concatenar: tkn = '", trim(tkn), "'"
                columnaa = columnaa + 1
                !!print *, "<<<<",trim(tkn)
                !!print *, ">>>>",trim(char)
            elseif (any(char == ayc) .and. contador <2) then
                tknaper = char
                contador = contador + 1
                columnaa = columnaa + 1
                !añade el lexema a la lista de lexemas
                numlex = numlex + 1
                palabra = "apertura"
                listal(numlex)%numerol = numlex
                listal(numlex)%lexemal = trim(tknaper)
                listal(numlex)%tipol = trim(palabra)
                listal(numlex)%columnal = columnaa
                listal(numlex)%lineal = lineaa
                
                tknaper = ""
                tkn = ""
            elseif (any(char == num)) then
                tkn = trim(tkn) // char
                columnaa = columnaa + 1
            
                
            else
                numErrores = numErrores + 1
                errores(numErrores,:) = (/ ichar(char), 69, columnaa, lineaa/)
                columnaa = columnaa + 1
                print *, "error"
            end if

            if (contador == 2) then
                
                estado = 5
                contador = 0
                !!print *, "contador reseteado"
            end if
            if (any(tkn == pr)) then
                numlex = numlex + 1
                palabra = "palabra reservada"
                listal(numlex)%numerol = numlex
                listal(numlex)%lexemal = trim(tkn)
                listal(numlex)%tipol = trim(palabra)
                listal(numlex)%columnal = columnaa
                listal(numlex)%lineal = lineaa
                tkn = ""
            
                if (tkn == "pais") then
                    
                elseif (tkn == "continente") then
                    estado = 3
                end if
            end if
            
        
        !el case 5 es para atributos de cada pais
        case (5)
            if (contadorpaises <4) then




            if (any(char == minus)) then
                !!print *, "Coincidencia en columna ", columnaa, " linea ", lineaa
                !!print *, "Antes de concatenar: tkn = '", trim(tkn), "' char = '", trim(char), "'"
                
                tkn = trim(tkn) // char
                !!print *, "Después de concatenar: tkn = '", trim(tkn), "'"
                columnaa = columnaa + 1
                !!print *, "<<<<",trim(tkn)
                !!print *, ">>>>",trim(char)
            elseif (any(char == ayc) .and. contador <2 ) then
                if (comillas .eqv. .true.) then
                    print *, "comillas"
                
                else

                tknaper = char
                contador = contador + 1
                columnaa = columnaa + 1
                !añade el lexema a la lista de lexemas
                numlex = numlex + 1
                palabra = "apertura"
                listal(numlex)%numerol = numlex
                listal(numlex)%lexemal = trim(tknaper)
                listal(numlex)%tipol = trim(palabra)
                listal(numlex)%columnal = columnaa
                listal(numlex)%lineal = lineaa
                endif

                
                if (any(tkn == pr)) then
                    print *, "token igual: ", tkn

                    numlex = numlex + 1
                    palabra = "palabra reservada"
                    listal(numlex)%numerol = numlex
                    listal(numlex)%lexemal = trim(tkn)
                    listal(numlex)%tipol = trim(palabra)
                    listal(numlex)%columnal = columnaa
                    listal(numlex)%lineal = lineaa
                    

                    if (tkn == "nombre") then
                        print *, "nombre"
                        tkn_pais = 1
                        

                        
                    elseif (tkn == "poblacion") then
                        print *, "poblacion"
                        tkn_pais = 2
                    elseif (tkn == "saturacion") then
                        print *, "saturacion"
                        tkn_pais = 3

                    elseif (tkn == "bandera") then
                        print *, "bandera"
                        tkn_pais = 4
                    else 
                    
                    endif
                    tkn = ""
                        
                        
                
                end if
                

                tknaper = ""
                
            endif


            




            !nombre
            if ((tkn_pais == 1) .and. (char .ne. ":" .and. char .ne. ";")) then
                
                if (any(char == minus) .or. any(char == mayus) .or. any(char == num) .or. char == comi .or. char == " ") then
                    if (char == " ") then
                        tkn = trim(tkn) // "-"
                        columnaa = columnaa + 1
                    elseif (char == comi) then
                        contadorc = contadorc + 1
                        columnaa = columnaa + 1
                        comillas = .true.


                        numlex = numlex + 1
                        palabra = "comillas"
                        listal(numlex)%numerol = numlex
                        listal(numlex)%lexemal = trim(char)
                        listal(numlex)%tipol = trim(palabra)
                        listal(numlex)%columnal = columnaa
                        listal(numlex)%lineal = lineaa


                    elseif (any(char == num)) then
                        tkn = trim(tkn) // char
                        columnaa = columnaa + 1
                    elseif (any(char == minus)) then
                        
                    else
                        tkn = trim(tkn) // char
                        columnaa = columnaa + 1
                    end if

                    
                    
                else
                    numErrores = numErrores + 1
                    errores(numErrores,:) = (/ ichar(char), 69, columnaa, lineaa/)
                    columnaa = columnaa + 1
                    print *, "errorzzz"
                end if

                if (contadorc == 2 .and. tkn .ne. "") then

                    numlex = numlex + 1

                    palabra = "cadena"
                    listal(numlex)%numerol = numlex
                    listal(numlex)%lexemal = trim(tkn)
                    listal(numlex)%tipol = trim(palabra)
                    listal(numlex)%columnal = columnaa
                    listal(numlex)%lineal = lineaa
                    !!print *, "token: ", tkn
                    tkn = ""

                    columnaa = columnaa + 1
                    comillas = .false.
                    contadorc = 0

                    
                end if

                if (contador == 2) then
                    contador = 0
                    tkn_pais = 0
                    contadorpaises = contadorpaises + 1

                    if (contadorpaises == 4) then
                        estado = 4
                        tkn_pais = 0
                        contadorpaises = 0
                        contadorsat = 0
                    endif

                end if
            


                !poblacion
            elseif (tkn_pais == 2 ) then
                
                if (any(char == num)) then
                    
                        tkn = trim(tkn) // char
                        columnaa = columnaa + 1
                elseif (any(char == ayc)) then
                    
                    
                else
                    numErrores = numErrores + 1
                    errores(numErrores,:) = (/ ichar(char), 69, columnaa, lineaa/)
                    columnaa = columnaa + 1
                    print *, "error"
                end if

                if (contador == 2) then

                    numlex = numlex + 1

                    palabra = "entero"
                    listal(numlex)%numerol = numlex
                    listal(numlex)%lexemal = trim(tkn)
                    listal(numlex)%tipol = trim(palabra)
                    listal(numlex)%columnal = columnaa
                    listal(numlex)%lineal = lineaa
                    !!print *, "token: ", tkn
                    tkn = ""

                    columnaa = columnaa + 1
                    comillas = .false.
                    contador = 0



                    tkn_pais = 0
                    contadorpaises = contadorpaises + 1
                    if (contadorpaises == 4) then
                        estado = 4
                        tkn_pais = 0
                        contadorpaises = 0
                        contadorsat = 0
                    endif

                end if
                
                print *, "poblacionnnnns ", tkn
                print *, "contador ", contador





            !saturacion
            elseif (tkn_pais == 3) then



                    if (any(char == num) .and. contadorsat<2) then
                    
                        tkn = trim(tkn) // char
                        columnaa = columnaa + 1
                        contadorsat = contadorsat + 1
                    elseif (any(char == ayc)) then
                        
                        
                    elseif (contadorsat == 2) then
                            
                            numlex = numlex + 1
        
                            palabra = "entero"
                            listal(numlex)%numerol = numlex
                            listal(numlex)%lexemal = trim(tkn)
                            listal(numlex)%tipol = trim(palabra)
                            listal(numlex)%columnal = columnaa
                            listal(numlex)%lineal = lineaa
                            !!print *, "token: ", tkn
                            tkn = ""
        
                            
                    else
                        numErrores = numErrores + 1
                        errores(numErrores,:) = (/ ichar(char), 69, columnaa, lineaa/)
                        columnaa = columnaa + 1
                        print *, "error"
                    end if

                    if (char == "%")  then
                        numlex = numlex + 1

                        palabra = "porcentaje"
                        listal(numlex)%numerol = numlex
                        listal(numlex)%lexemal = trim(char)
                        listal(numlex)%tipol = trim(palabra)
                        listal(numlex)%columnal = columnaa
                        listal(numlex)%lineal = lineaa
                    endif
                    if (contador == 2) then

                        tkn = ""

                        columnaa = columnaa + 1
                        comillas = .false.
                        contador = 0
                        contadorpaises = contadorpaises + 1

                        if (contadorpaises == 4) then
                            estado = 4
                            tkn_pais = 0
                            contadorpaises = 0
                            contadorsat = 0
                        endif


                        tkn_pais = 0
                    endif   
                

            

        elseif (tkn_pais == 4 .and. char .ne. ";")  then
            if (char == comi .and. contadorc <2) then
                comillas = .true.
            endif
            print *, "char: ", char
            print *, "comillas: ", comillas
            if ((char == ':') .and. (comillas .eqv. .false.)) then

                print *, "bandera"
            else

            if (any(char == minus) .or. any(char == mayus) .or. any(char == num) .or. char == comi .or. char == " ".or. & 
            char == "\" .or. char == "." .or. char == ":") then
                if (char == " ") then
                    tkn = trim(tkn) // "-"
                    columnaa = columnaa + 1
                elseif (char == comi) then
                    contadorc = contadorc + 1
                    columnaa = columnaa + 1
                    comillas = .true.

                    numlex = numlex + 1
                    palabra = "comillas"
                    listal(numlex)%numerol = numlex
                    listal(numlex)%lexemal = trim(char)
                    listal(numlex)%tipol = trim(palabra)
                    listal(numlex)%columnal = columnaa
                    listal(numlex)%lineal = lineaa







                elseif (any(char == num)) then
                    tkn = trim(tkn) // char
                    columnaa = columnaa + 1
                elseif (any(char == minus)) then
                    
                else
                    tkn = trim(tkn) // char
                    columnaa = columnaa + 1
                end if
                
            else
                numErrores = numErrores + 1
                errores(numErrores,:) = (/ ichar(char), 69, columnaa, lineaa/)
                columnaa = columnaa + 1
                print *, "errorzzz"
            end if
            
            if (contadorc == 2 .and. tkn .ne. "") then

                numlex = numlex + 1

                palabra = "cadena"
                listal(numlex)%numerol = numlex
                listal(numlex)%lexemal = trim(tkn)
                listal(numlex)%tipol = trim(palabra)
                listal(numlex)%columnal = columnaa
                listal(numlex)%lineal = lineaa
                !!print *, "token: ", tkn
                tkn = ""

                columnaa = columnaa + 1
                comillas = .false.
                contadorc = 0

                
            end if

            if (contador == 2) then
                contador = 0
                tkn_pais = 0
                contadorpaises = contadorpaises + 1

                if (contadorpaises == 4) then
                    estado = 4
                    tkn_pais = 0
                    contadorpaises = 0
                    contadorsat = 0
                endif
            end if































        end if

        endif

        else 
      
            

        endif

        end select
    puntero = puntero + 1
    end if



    

    end do

    if(numErrores > 0) then
        print *, "Errores encontrados: ", numErrores
        do i=1,numErrores
            char_error = achar(errores(i,1))
            print *, "error en caracter ", char_error, " Descripcion",errores(i,4) , &
            " en la columna ", errores(i,3), " en la linea " , errores(i,4)
        end do
    end if
    print *, "//////////////////////////////////////////////"
    do i=1,numlex
        
        print *, "Num ", listal(i)%numerol, " Lex: ", trim(listal(i)%lexemal), " ", listal(i)%tipol,&
        " c", listal(i)%columnal, " f", listal(i)%lineal
        print *, ""
    end do


    
end program analizador

