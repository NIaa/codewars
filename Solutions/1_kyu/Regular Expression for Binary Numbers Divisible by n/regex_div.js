/*
    todo: simplify regex generated 
    length, (), |, *
    or write a parser to deal with the result
*/
function regexDivisibleBy(n) {
    if(n==1) return '^(0|1)+$'
    exp = 0;
    while(n%2==0) { n/=2; exp+=1 }
    m = []; for(var i=0; i<n; i++) { m[i] = []; for(var j=0; j<n; j++) m[i][j] = ''; }
    for(var i=0; i<n; i++) {
        m[i][i*2%n] = '0'
        m[i][(i*2+1)%n] = '1'
    }
    for(var i=0; i<n; i++) {
        m[i][n-1] = ''
        m[n-1][i] = ''
    }
    if(n%2==1) m[(n-1)/2][n-2] = '01*0'        
    else       m[  n/2-1][n-2] = '11*0'

    buf_i = []; buf_o = []; 
    for(var k=1; k<n-1; k++) {
        for(var i=0; i<n; i++) { buf_i[i] = ''; buf_o[i] = '' }
        for(var i=0; i<n; i++) {
            if(m[i][k] != '') { buf_i[i] = m[i][k]; m[i][k] = '';}
            if(m[k][i] != '') { buf_o[i] = m[k][i]; m[k][i] = '';}
        }
        for(var i=0; i<n; i++) {
            if(buf_i[i] != '') {
                for(var j=0; j<n; j++) {
                    if(buf_o[j] != '') {
                        if(buf_i[k] == '') {
                            if(m[i][j] == '') {
                                m[i][j] = '(' + buf_i[i] + ')(' + buf_o[j] + ')'; 
                            }
                            else { 
                                m[i][j] = '(' + m[i][j] + ')|(' + buf_i[i] + ')(' + buf_o[j] + ')'
                            }//(m[i][j]) or not
                            
                        }
                        else if(i!=k){
                            if(m[i][j] == '') {
                                m[i][j] = '(' + buf_i[i] + ')(' + buf_i[k] + ')*(' + buf_o[j] + ')'
                            }
                            else {
                                m[i][j] = '(' + m[i][j] + ')|(' + '(' + buf_i[i] + ')(' + buf_i[k] + ')*(' + buf_o[j] + '))'
                            }
                        }; 
                    }
                }
            }
        }
    }
    if(exp==0) return '^('+m[0][0]+')+$'
    else if(n==1) return '^' + '(0|1)*' + Array(exp+1).join('0') + '$' 
    else return '^' + '(' + m[0][0] + ')*' + Array(exp+1).join('0') + '$'
}