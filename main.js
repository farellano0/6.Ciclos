class App{

    _factorial(x){
        let total = 1;
        for(let i = 1; i <= x; i++){
            total *= i;
        }
        return total;
    }

    serieUno(number){
        let resultado = 0;
        let denominador = 1;

        for(let i = 1; i >= 1/this._factorial(number); i = 1/this._factorial(denominador++)){
            resultado += i;
        }

        return resultado.toFixed(4);
    }

    serieDos(number){
        let resultado = 0;
        let denominador = 1;
        let posición = 1
        let signo = true;
        let i = 4;

        do{
            if(signo){
                resultado += i;
                signo = false;
            } else {
                resultado -= i;
                signo = true;
            }

            denominador += 2
            i = 4/(denominador);
            posición++;
        } while (posición <= number);

        return resultado.toFixed(4);
    }
}

let app = new App();

console.log(`e(5) = ${app.serieUno(5)}`);
console.log(`s(9) = ${app.serieDos(9)}`);