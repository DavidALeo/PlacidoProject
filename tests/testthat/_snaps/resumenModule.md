# resumenModuleUI() creates expected HTML

    Code
      resumenModuleUI("a", "b")
    Output
      <div class="row">
        <div class="col-sm-4">
          <form class="well" role="complementary">
            <div class="card-body html-fill-item html-fill-container" style="flex:1 1 auto; margin-top:auto;margin-bottom:auto;">
              <h2>Resumen</h2>
              Datos necesarios para continuar:
              <div class="form-group shiny-input-container">
                <label class="control-label" id="a-BatchSize-label" for="a-BatchSize">Tamaño del lote</label>
                <input id="a-BatchSize" type="number" class="form-control" value="0"/>
              </div>
              <div class="form-group shiny-input-container">
                <label class="control-label" id="a-LabeledQuantity-label" for="a-LabeledQuantity">Cantidad nominal</label>
                <input id="a-LabeledQuantity" type="number" class="form-control" value="0"/>
              </div>
            </div>
          </form>
        </div>
        <div class="col-sm-8" role="main">
          <h2>Resumen</h2>
          <div id="a-textualExplanation" class="shiny-html-output"></div>
        </div>
      </div>

