//FUNÇÃO PARA MONTAR OS GRÁFICOS DE REVIEWS.
 // PARAMETRO: empresa selecionada no combo
function chartsReviews(companySelect){
  console.log('Charts Reviews')
  $('.card-body > *').empty()

      // CAPTURANDO REVIEWS DA LISTA 'dados'
      data = dados.reviews

/******** INFORMAÇÕES NUMÉRICAS (BOXES) ****************************/
      var div2         = d3.select('#boxtworeview')
      var countCompany = group(data,['company'])
      var scaleColor   = d3.scaleSequential(d3["interpolateBlues"]).domain([-1, 5]);

      d3.select('.boxtworeview .card-body-icon').append('i').classed('fa fa-fw fa-building', true)
      d3.select('.boxtworeview').style('background-color', scaleColor(2))//ALTERANDO A COR DA DIV. SERÁ UTILIZADA ESCALA DE COR AZUL
      d3.select('#selcompany').style('background-color', scaleColor(2))//ALTERANDO A COR DA DIV. SERÁ UTILIZADA ESCALA DE COR AZUL
     

      $('#selcompany > *').remove();
      $('#selcompany').append($('<option>',{
          value: 'Todos',
          text: 'Todos'
      })) 
      $.each(countCompany,function(x,y){
        $('#selcompany').append($('<option>',{
          value: y.value,
          text: y.value
        }))
      })
      if(companySelect != ''){
        $('#selcompany').val(companySelect);
      }

      boxNumber2.call(div2, {docs: countCompany, title: "Companies"})

      data = data.filter(function(x) {
        if(companySelect == 'Todos' || companySelect == ''){
          return x
        }else{
          return x.company == companySelect
        }
      })

      var div1 = d3.select('#boxonereview')
      d3.select('.boxonereview .card-body-icon').append('i').classed('fa fa-fw fa-database', true)
               d3.select('.boxonereview').style('background-color', scaleColor(1))

      boxNumber2.call(div1, {docs: data, title: "Reviews"})

      
      var div3         = d3.select('#boxfourreview')

      //CALCULANDO AS MÉTRICAS DE RATING(media, soma, min, max)
      var metricRating = groupMetric(data,'company','rating')
      var title1 = 'Mean Rating' //TITULO DA DIV
      d3.select('.boxthirdreview .card-body-icon').append('i').classed('fa fa-fw fa-calculator ', true)   
      d3.select('.boxthirdreview').style('background-color', scaleColor(3))

      var format = d3.format(".3n");
      
      var avg = d3.mean(metricRating, function(x){ return x.count.avg })

      // INSERINDO INFORMAÇÕES NA DIV COM EFEITOS DE TRANSITIONS
      div3.append('span')   
          .transition()
          .duration(1500)
          .on("start", function repeat() {
            d3.active(this)
              .tween("text", function() {
                  var that = d3.select(this),
                      i = d3.interpolateNumber(that.text().replace(/,/g, ""), avg);
                  return function(t) { that.text( title1 + ' ' + format(i(t))); };
              })
              .transition()
              .delay(2000)
          });

      var div4 = d3.select('#boxthirdreview')
      d3.select('.boxfourreview .card-body-icon').append('i').classed('fa fa-fw fa-building', true)  
      d3.select('.boxfourreview').style('background-color', scaleColor(4))

      var title2 = 'Min Rating'
      var min = d3.min(metricRating, function(x){ return x.count.min})
      
      div4.append('span')   
                .transition()
                .duration(1500)
                .on("start", function repeat() {
                  d3.active(this)
                    .tween("text", function() {
                        var that = d3.select(this),
                            i = d3.interpolateNumber(that.text().replace(/,/g, ""), min);
                        return function(t) { that.text( title2 + ' ' + format(i(t))); };
                    })
                    .transition()
                    .delay(2000)
                });

      var title3 = 'Max Rating'
      var max = d3.max(metricRating, function(x){ return x.count.max})
      div4.append('br')
      div4.append('span')   
                .transition()
                .duration(1500)
                .on("start", function repeat() {
                  d3.active(this)
                    .tween("text", function() {
                        var that = d3.select(this),
                            i = d3.interpolateNumber(that.text().replace(/,/g, ""), max);
                        return function(t) { that.text( title3 + ' ' + format(i(t))); };
                    })
                    .transition()
                    .delay(2000)
                });


      var dateUpdate = group(data, ['dateCollect'])
      dateUpdate.sort(function(x, y){        
        return d3.descending(x.value,y.value);
      })
      
      var divUpdate = d3.selectAll('.card-footer span').text(dateUpdate[0].value)


/******** GRÁFICO DE BARRAS POSITIONS ****************************/
      w = parseInt(d3.select('#positionchart').style('width')) * 0.9 ,
      h = parseInt(d3.select('#positionchart').style('width')) * 0.25;

      
      var barPosition = initVar.call(null, {id: '#positionchart', titleDiv: "", class: '', width: w, height:h})    
      
      var x = d3.scaleBand().range([0,barPosition.margins.width]).padding(0.1),
          //x = d3.scaleLinear().rangeRound([0,barPosition.margins.width]),
          y = d3.scaleLinear().rangeRound([barPosition.margins.height,0])
          //y = d3.scaleBand().range([barPosition.margins.height, 0]).padding(0.1);

            
      var groupPosit = group(data,['position'])
      groupPosit = groupPosit.filter(function(x){ return x.value != ''})

      
      groupPosit.sort(function(x, y){        
        return y.count - x.count;
      })
  
      groupPosit = groupPosit.slice(0,10)
    
      barChart.call(barPosition.g, {data: groupPosit, margins: barPosition, axis: {x : x, y : y},horizontal: false, setFill: true } )  

/******** GRÁFICO DE BARRA DATE(YEAR) ****************************/
      w = parseInt(d3.select('#barchartreviews').style('width')) * 0.9 ,
      h = parseInt(d3.select('#barchartreviews').style('width')) * 0.25;

      var bar = initVar.call(null, {id: '#barchartreviews', titleDiv: "", class: '', width: w, height:h})    
      
      var x = d3.scaleBand().range([0,bar.margins.width]).padding(0.1),
          //x = d3.scaleLinear().rangeRound([0,bar.margins.width]),
          y = d3.scaleLinear().rangeRound([bar.margins.height,0])
          //y = d3.scaleBand().range([bar.margins.height, 0]).padding(0.1);

      var years = data.map(function(x){
        return {value: new Date(x.date).getFullYear() }
      })
            
      groupYear = group(years,['value'])
      groupYear = groupYear.filter(function(x){ return x.value != 'NaN'})
      
      groupYear.sort(function(x, y){        
        return x.value - y.value;
      })


      barChart.call(bar.g, {data: groupYear, margins: bar, axis: {x : x, y : y},horizontal: false, setFill: true } )

/******** GRÁFICO DE PIZZA RECOMMENDS ****************************/
      var pieRecommend = initVar.call(null, {id: '#pierecommend', titleDiv: "", class: '', width: '300', height:'250'})    
      
      var groupRecommend = group(data,['recommend'])
      groupRecommend = groupRecommend.filter(function(x) {return x.value != "" })

      pieChar.call(pieRecommend.g, {data: groupRecommend, margins: pieRecommend})

/******** GRÁFICO TREEMAP CITY ****************************/
      w = parseInt(d3.select('#treemapreview').style('width')) * 0.9,
      h = parseInt(d3.select('#treemapreview').style('height')) * 12
      var barCity = initVar.call(null, {id: '#treemapreview', titleDiv: "", class: 'treemap', width: w, height:h})    

      var groupCity = group(data, ['city'])
            
      var x = d3.scaleBand().range([0,barCity.margins.width]).padding(0.1),//d3.scaleLinear().rangeRound([0,vars.margins.width]),
          y = d3.scaleLinear().rangeRound([barCity.margins.height,0]);//d3.scaleBand().range([vars.margins.height, 0]).padding(0.1);

      groupCity.sort(function(x,y){
        return y.count - x.count;
      })
      groupCity = groupCity.filter(function(x) {return x.value != 'NI'}).slice(0,20)
      
      treeMap.call(barCity.g, {data: groupCity, margins: barCity.margins})

/******** GRÁFICO DE BOLHAS PROS ****************************/
      w = parseInt(d3.select('#bubblepros').style('width')) * 0.9,
      h = parseInt(d3.select('#bubblepros').style('height')) * 9

      var bubPros = initVar.call(null, {id: '#bubblepros', titleDiv: "", class: '', width: w, height:h})

      var arrayPros = []
      data.map(function(x){
        x.pros.split(' ').map(function(d){
          arrayPros.push({pros:d})
        })
      })
      var groupProsComents = group(arrayPros,['pros'])
      groupProsComents = groupProsComents.filter(function(x) {return x.value != "" })
      
      
      pct = 0.20
      // RECUPERANDO % DAS PALAVRAS
      var mediaTermos = d3.max(groupProsComents, function(x){ return x.count})*pct
      console.log('extent = ', mediaTermos)
      

      //groupProsComents = groupProsComents.slice(0,150)
      
      groupProsComents = groupProsComents.filter(function(x){ return x.count > (mediaTermos)})
      groupProsComents.sort(function(x,y){
        return y.count - x.count
      })

      bubPros.margins.margin.bottom = 100;
      bubPros.margins.margin.left = 30;

      //INSERINDO ATRIBUTOS NO ELEMENTO
      bubPros.g.attr("text-anchor","middle").attr('font-family',"sans-serif").attr('font-size',"10")
      bubbleChart.call(bubPros.g, {data: groupProsComents,margins: bubPros.margins, setColor: 'interpolateGreens'})

/******** GRÁFICO DE BOLHAS CONS ****************************/
      w = parseInt(d3.select('#bubblecons').style('width')) * 0.9,
      h = parseInt(d3.select('#bubblecons').style('height')) * 9

      var bubCons = initVar.call(null, {id: '#bubblecons', titleDiv: "", class: '', width: w, height:h})

      var arrayCons = []
      data.map(function(x){
        x.cons.split(' ').map(function(d){
          arrayCons.push({pros:d})
        })
      })
      var groupConsComents = group(arrayCons,['pros'])
      groupConsComents = groupConsComents.filter(function(x) {return x.value != "" })
      //groupConsComents = groupConsComents.slice(0,150)
      
      mediaTermos = d3.max(groupConsComents, function(x){ return x.count})*pct
      
      groupConsComents = groupConsComents.filter(function(x){ return x.count > (mediaTermos)})
      
      groupConsComents.sort(function(x,y){
        return y.count - x.count
      })

      bubCons.margins.margin.bottom = 100;
      bubCons.margins.margin.left = 30;

      bubCons.g.attr("text-anchor","middle").attr('font-family',"sans-serif").attr('font-size',"10")
      bubbleChart.call(bubCons.g, {data: groupConsComents,margins: bubCons.margins, setColor: 'interpolateReds'})


/******** GRÁFICO PIE PREDIC RECOMMEND ****************************/
      recomPred = dados.predRecom

      
      var pieRecommend = initVar.call(null, {id: '#piepredrecommend', titleDiv: "", class: '', width: '300', height:'250'})    
      
      var groupRecommend = group(recomPred,['predRecom'])
      groupRecommend = groupRecommend.filter(function(x) {return x.value != "" })

      pieChar.call(pieRecommend.g, {data: groupRecommend, margins: pieRecommend})


} // FIM FUNÇÃO CHARTSREVIEWS
