// FUNÇÃO PARA MONSTAR OS GRAFICOS DO DASHBOARDS DO MENU 'JOBS VACANCIES'
function chartsJobs(countrySelect){
  console.log('Charts Jobs')
  $('.card-body > *').empty() // LIMPANDO OS GRÁFICOS

  //d3.csv('data/clusterReq2.csv', function(error, data){
      
      cluster = dados.clusterReq;

      // FILTRANDO OS DADOS DE ACORDO COM O PAÍS SELECIONADO 
      cluster = cluster.filter(function(x) {  
        if(countrySelect == 'Todos' || countrySelect == ''){
          return x
        }else{
          return x.country == countrySelect
        }
      })

      // FUNÇÃO PARA MONTAR O MAPA
      $.getJSON('data/t5.geojson', function (geojson) {
          Highcharts.mapChart('clustermap', {

              chart: {
              spacingBottom: 20
          },
          title: {
              text: ''
          },

          legend: {
              enabled: true
          },

           mapNavigation: {
              enabled: true
          },
          plotOptions: {
              map: {
                  allAreas: false,
                  joinBy: ['sigla', 'code'],
                  dataLabels: {
                      enabled: true,
                      color: '#FFFFFF',
                      formatter: function () {
                          if (this.point.properties ) {
                              var label = this.point.properties['sigla'].replace('_br','')
                              return label;
                          }
                      },
                      format: null,
                      style: {
                          fontWeight: 'bold'
                      }
                  },
                  mapData: geojson,
                  tooltip: {
                      headerFormat: '',
                      pointFormat:  '{point.name}: <b>{series.name}</b>'
                  }
              }
          },
          series: [{
              name: 'Cluster 1',
              data: cluster.filter(function(x) {  return x.Cluster == '1'}).map(function(y){ return {code: y.state}})
            },{
              name: 'Cluster 2',
              data: cluster.filter(function(x) {  return x.Cluster == '2'}).map(function(y){ return {code: y.state}})
           },{
              name: 'Cluster 3',
              data: cluster.filter(function(x) {  return x.Cluster == '3'}).map(function(y){ return {code: y.state}})
           },{
              name: 'Cluster 4',
              data: cluster.filter(function(x) {  return x.Cluster == '4'}).map(function(y){ return {code: y.state}})
           }
          ]
        });
      });
  //});


      // PEGANDO OS DADOS jobs DA LISTA 'dados'
      var data = dados.jobs


/******** MONTANDO INFORMAÇÕES NÚMERICAS NOS BOXES  ****************************/
      //SELECIONANDO DIV 
      var div2 = d3.select('#boxnumbertwo')

      // AGRUPANDO PAÍSES E FAZENDO A CONTAGEM
      var countCountry = group(data,['country'])
      d3.select('.boxtwo .card-body-icon').append('i').classed('fa fa-fw fa-globe', true)
      
      $('#selcountry > *').remove();
      $('#selcountry').append($('<option>',{
          value: 'Todos',
          text: 'Todos'
      })) 
      $.each(countCountry,function(x,y){
        $('#selcountry').append($('<option>',{
          value: y.value,
          text: y.value
        }))
      })
      if(countrySelect != ''){
        $('#selcountry').val(countrySelect);
      }

      boxNumber2.call(div2, {docs: countCountry, title: "Countries"})
      

      data = data.filter(function(x) {
        if(countrySelect == 'Todos' || countrySelect == ''){
          return x
        }else{
          return x.country == countrySelect
        }
      })


      var div1 = d3.select('#boxnumber')
      
      d3.select('.boxone .card-body-icon').append('i').classed('fa fa-fw fa-database', true)
      //CHAMANDO A FUNÇÃO PARA MONTAR BOX 
      boxNumber2.call(div1, {docs: data, title: "Jobs Vacancies"})

      //Number of jobs in initial page
      d3.select('.numbjobs').text(data.length.toLocaleString('de-DE'))

      //SELECTIONANDO DIV
      var div2 = d3.select('#boxnumberthird')
      
      // AGRUPANDO CIDADES E FAZENDO CONTAGEM
      var countCities = group(data,['city'])
      d3.select('.boxthird .card-body-icon').append('i').classed('fa fa-fw fa-location-arrow ', true)   
      //CHAMANDO A FUNÇÃO PARA MONTAR BOX
      boxNumber2.call(div2, {docs: countCities, title: "Cities"})

      var div2 = d3.select('#boxnumberfour')
      d3.select('.boxfour .card-body-icon').append('i').classed('fa fa-fw fa-building', true)   

      //AGRUPANDO POSITION E FAZENDO CONTAGEM
      var countPositions = group(data,['position'])
      boxNumber2.call(div2, {docs: countPositions, title: "Positions"})

      // PEGANDO A DATA MAIS ATUAL DE COLETA DOS DADOS
      var dateUpdate = group(data, ['dateCollect'])
      dateUpdate.sort(function(x, y){        
        return d3.descending(x.value,y.value);
      })
      var divUpdate = d3.selectAll('.card-footer span').text(dateUpdate[0].value)
      
      

/******** GRAFICO DE LINHA ****************************/
      //DEFININDO O VALOR DE w E h DE ACORDO COM O TAMANHO DA DIV (90% e 7x maior)
      // DEFINIDO ASSIM PARA MONTAR OS GRÁFICOS RESPONSIVOS
      var w = parseInt(d3.select('#linechart').style('width')) *0.90,
          h = parseInt(d3.select('#linechart').style('height')) *7
          if(h == 0){
             h = w * 0.2
          }

      // CHAMANDO FUNÇÃO PARA INICIAR AS VARIAVEIS NECESSÁRIAS PARA MONTAR O GRÁFICO
        // PASSANDO COMO PARAMETRO A id DIV, TITULO DA TIVE, LARGURA E ALTURA DO GRAFICO
      var line  = initVar.call(null, {id: '#linechart', titleDiv: "", class: '', width: w, height: h})
      
      //AGRUPANDO OS DADOS POR DATA
      groupDate = group(data,['date'])

      //CONVERTENDO AS DATAS DE STRING PARA DATE
      groupDate.map(function(x){
        x.value = parseTime(x.value)
      })
      
      // ORDENAÇÃO DAS DATAS
      groupDate.sort(function(x, y){        
        return y.value - x.value;
      })

      //CHAMANDO FUNÇÃO PARA CRIAR O GRÁFICO DE LINHAS
        //PARAMETROS: g, dados e as margins
      lineChart.call(line.g, {data:groupDate, margins: line.margins})

/******** GRAFICO DE BOLHAS ****************************/
      w = parseInt(d3.select('#bubblechart').style('width')) * 0.9,
      h = parseInt(d3.select('#bubblechart').style('height')) * 9

      var bolha = initVar.call(null, {id: '#bubblechart', titleDiv: "", class: '', width: w, height:h})

      //CRIANDO ARRAY PARA ARMAZENAR SKILL SEPARADOS
      var arraySkill = []

      //FAZENDO SLIPT DA SKILLS
      data.map(function(x){
        x.skills.split(' ').map(function(d){
          arraySkill.push({skill:d})
        })
      })
      var groupSkill = group(arraySkill,['skill'])

      //RETORNANDO REGISTROS QUE NÃO SEJA VAZIO
      groupSkill = groupSkill.filter(function(x) {return x.value != "" })
      
      // RETORNANDO REGISTROS QUE TENHAM CONTAGEM ACIMA DE 15
      groupSkill = groupSkill.filter(function(x){return x.count > 15})

      //ORDENANDO O ARRAY DE SKILLS
      groupSkill.sort(function(x,y){
        return y.count - x.count
      })

      // AJUSTANDO AS MARGINS DA DIV QUE SERÁ CRIADO O GRÁFICO DE BOLHAS
      bolha.margins.margin.bottom = 100;
      bolha.margins.margin.left = 30;

      // INSERINDO ATRIBUTOS DE FORMATAÇÃO NO ELEMENTO DO GRÁFICO DE BOLHAS 
      bolha.g.attr("text-anchor","middle").attr('font-family',"sans-serif").attr('font-size',"10")
      bubbleChart.call(bolha.g, {data: groupSkill,margins: bolha.margins})

/******** GRAFICO DE BARRAS ****************************/
      //CAPTURANDO O TAMANHO DA DIV
      w = parseInt(d3.select('#barchart').style('width')) * 0.9 ,
      h = parseInt(d3.select('#barchart').style('width')) * 0.5;//É UTILIZADA O WIDTH PORQUE O HEIGTH NÃO É DEFINIDO 

      
      var bar = initVar.call(null, {id: '#barchart', titleDiv: "", class: '', width: w, height:h})    
      
      var groupJobs = group(data, ['position'])
      
      
      var x = d3.scaleBand().range([0,bar.margins.width]).padding(0.1),
          //x = d3.scaleLinear().rangeRound([0,bar.margins.width]),
          y = d3.scaleLinear().rangeRound([bar.margins.height,0])
          //y = d3.scaleBand().range([bar.margins.height, 0]).padding(0.1);

      groupJobs.sort(function(x,y){
        return y.count - x.count;
      })
      groupJobs = groupJobs.slice(0,5)
      
      //CHAMANDO FUNÇÃO QUE MONTA O GRÁFICO DE BARRAS
        //PARAMETROS: elemento G, dados, marnins, axis, se as barras serão horizontal(true) ou vertical(false) E se as barras serão de cor sólida de acordo com CSS criado
          //se FALSE irá usar uma escala de cor (azul)
      barChart.call(bar.g, {data: groupJobs, margins: bar, axis: {x : x, y : y},horizontal: false, setFill: true } )

/******** GRAFICO DE PIZZA LANGUAGES****************************/
      var pie = initVar.call(null, {id: '#piechart', titleDiv: "", class: '', width: '300', height:'250'})    
      

      var arrayLang = []
      data.map(function(x){      
        x.language.split(' ').map(function(d){      
          arrayLang.push({language:d})
        })
      })
      var groupLang = group(arrayLang,['language'])
      groupLang = groupLang.filter(function(x) {return x.value != "" })

      pieChar.call(pie.g, {data: groupLang, margins: pie})

/******** GRAFICO DE PIZZA EDUCATION ****************************/
      var pieEdu = initVar.call(null, {id: '#piecharttwo', titleDiv: "", class: '', width: '300', height:'250'})    

      var arrayEdu = []
      data.map(function(x){      
        x.education.split(' ').map(function(d){      
          arrayEdu.push({education:d})
        })
      })
      var groupLang = group(arrayEdu,['education'])
      groupLang = groupLang.filter(function(x) {return x.value != "" })

      pieChar.call(pieEdu.g, {data: groupLang, margins: pieEdu})



/******** GRAFICO TREEMAP ****************************/
      w = parseInt(d3.select('#treemap').style('width')) * 0.9,
      h = parseInt(d3.select('#treemap').style('height')) * 12
      var barCity = initVar.call(null, {id: '#treemap', titleDiv: "", class: 'treemap', width: w, height:h})    

      var groupCity = group(data, ['city'])
      
      var x = d3.scaleBand().range([0,barCity.margins.width]).padding(0.1),
          y = d3.scaleLinear().rangeRound([barCity.margins.height,0]);

      groupCity.sort(function(x,y){
        return y.count - x.count;
      })
      groupCity = groupCity.filter(function(x) {return x.value != 'ND'}).slice(0,20)
      
      treeMap.call(barCity.g, {data: groupCity, margins: barCity.margins})  

} //FIM FUNÇÃO chartsjobs
