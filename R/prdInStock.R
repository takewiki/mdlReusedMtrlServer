#' 回料入库
#'
#' @param file_name 文件名
#' @param token 口令
#' @param overWrite 是否覆盖
#'
#' @return 返回值
#' @export
#'
#' @examples
#' prdInStock_read()
prdInStock_read <- function(file_name="data-raw/回料粉碎记录表模板.xlsx",
                            token = '9B6F803F-9D37-41A2-BDA0-70A7179AF0F3',
                            overWrite = FALSE) {
  data <- readxl::read_excel(file_name,
                     col_types = c("numeric", "text", "text",
                                   "date", "text", "text", "text", "text",
                                   "numeric", "text", "text", "text"))
  #以回料编码作为依据，删除不完整记录
  data = data[complete.cases(data$`回料编码`), ]
  ncount = nrow(data)
  if(ncount >0){
    names(data) <-c('FSeq',
                    'FCompanyName',
                    'FWorkshop',
                    'FDate',
                    'FShiftName',
                    'FPrdNumber' ,
                    'FPrdName' ,
                    'FColor' ,
                    'FQty',
                    'FSource',
                    'FOperator',
                    'FNote' )
    data$FSeq <-1:ncount
    max_id = tsda::db_maxId2(token = token,FTableName = 'rds_prd_reusedMtrl_prdInStock')
    data$FInterId <- 1:ncount + max_id
    col_name <-c('FInterId','FSeq',
                 'FCompanyName',
                 'FWorkshop',
                 'FDate',
                 'FShiftName',
                 'FPrdNumber' ,
                 'FPrdName' ,
                 'FColor' ,
                 'FQty',
                 'FSource',
                 'FOperator',
                 'FNote' )
    data <- data[ ,col_name]
    conn = tsda::sql_getConn(token = token)
    if(overWrite){
      #覆盖
      #写入临时表
      tsda::db_writeTable(conn = conn,table_name = 'rds_prd_reusedMtrl_prdInStockInput',r_object = data,append = T)
      #写入备份表
      sql_bak <- paste0("insert into rds_prd_reusedMtrl_prdInStockDel
select a.* from rds_prd_reusedMtrl_prdInStock a
inner join rds_prd_reusedMtrl_prdInStockInput b
on a.FCompanyName = b.FCompanyName
and  a.FWorkshop =  b.FWorkshop
and a.FDate =  b.FDate
and a.FShiftName = b.FShiftName
and a.FPrdNumber = b.FPrdNumber")
      tsda::sql_update(conn,sql_bak)
      #删除记录表
      sql_del <- paste0("delete a  from rds_prd_reusedMtrl_prdInStock a
inner join rds_prd_reusedMtrl_prdInStockInput b
on a.FCompanyName = b.FCompanyName
and  a.FWorkshop =  b.FWorkshop
and a.FDate =  b.FDate
and a.FShiftName = b.FShiftName
and a.FPrdNumber = b.FPrdNumber")
      tsda::sql_update(conn,sql_del)
      #插入数据
      sql_ins <- paste0("insert into rds_prd_reusedMtrl_prdInStock
select * from rds_prd_reusedMtrl_prdInStockInpu")
      tsda::sql_update(conn,sql_ins)
      #清除临时表
      tsda::db_truncateTable(token = token,table_name = 'rds_prd_reusedMtrl_prdInStockInput')

    }else{
      #新增
      tsda::db_writeTable(conn = conn,table_name = 'rds_prd_reusedMtrl_prdInStock',r_object = data,append = T)


    }





  }
  return(data)

}


#' 查询数据
#'
#' @param token 口令
#' @param FStartDate 开始日期
#' @param FEndDate 结束日期
#' @param FCompanyName 公司名称
#' @param FWorkshop 车间
#' @param FShiftName 班次
#'
#' @return
#' @export
#'
#' @examples
prdInStock_query <- function( token = '9B6F803F-9D37-41A2-BDA0-70A7179AF0F3',
                              FStartDate = '2022-01-01',
                              FEndDate = '2022-01-01',
  FCompanyName ='苏州赛普生物科技有限公司' ,
                             FWorkshop = '生产部',

                             FShiftName ='白班'
                             ){
  if(FCompanyName == ''){
    sql_FCompanyName = "  "
  }else{
    sql_FCompanyName = paste0(" and   FCompanyName ='",FCompanyName,"' ")
  }

  if(FWorkshop == ''){
    sql_FWorkshop = "  "
  }else{
    sql_FWorkshop = paste0(" and  FWorkshop = '",FWorkshop,"' ")
  }

  if(FShiftName == ''){
    sql_FShiftName = "  "
  }else{
    sql_FShiftName = paste0(" and  FShiftName ='",FShiftName,"' ")
  }


  sql <- paste0("SELECT
       [FSeq] 序号
      ,[FCompanyName] 公司全称
      ,[FWorkshop]  车间
      ,[FDate]  日期
      ,[FShiftName]   班次
      ,[FPrdNumber]  回料编码
      ,[FPrdName]  回料名称
      ,[FColor]  颜色
      ,[FQty]  [粉碎重量（KG）]
      ,[FSource]  来源
      ,[FOperator]  责任人
      ,[FNote]  备注
  FROM rds_prd_reusedMtrl_prdInStock
  where    FDate >= '",FStartDate,"' and FDate <='",FEndDate,"' ",sql_FCompanyName,sql_FWorkshop,sql_FShiftName)
  print(sql)
  conn = tsda::sql_getConn(token = token)
  data = tsda::sql_select(conn,sql)
  return(data)


}


#' 回料入库响应函数
#'
#' @param input 输入
#' @param output 输出
#' @param session 会话
#' @param token 口令
#'
#' @return 返回值
#' @export
#'
#' @examples
#' prdInstockServer()
prdInstockServer <- function(input,output,session,token='9B6F803F-9D37-41A2-BDA0-70A7179AF0F3') {
  print(1)
  var_txtPrdInStock_company <- tsui::var_text('txtPrdInStock_company')
  var_txtPrdInStock_workshop <- tsui::var_text('txtPrdInStock_workshop')
  var_txtPrdInStock_shiftNo <- tsui::var_text('txtPrdInStock_shiftNo')
  var_datePrdInStock_queryDates <- tsui::var_dateRange('datePrdInStock_queryDates')
  shiny::observeEvent(input$btnprdInStock_query,{

    #查询
    print(2)
    dates = var_datePrdInStock_queryDates()
    print(dates)
    FStartDate = as.character(dates[1])
    FEndDate = as.character(dates[2])
    FCompanyName = as.character(var_txtPrdInStock_company())
    FWorkshop = as.character(var_txtPrdInStock_workshop())
    FShiftName = var_txtPrdInStock_shiftNo()


    print(3)
    data = prdInStock_query(token =token ,FStartDate =  FStartDate,FEndDate = FEndDate,
                            FCompanyName = FCompanyName,FWorkshop = FWorkshop,FShiftName = FShiftName)
    print(4)
    file_name = paste0('回料粉碎记录表',tsdo::getTime(),'.xlsx')
    #设置显示
    tsui::run_dataTable2(id = 'dataviewprdInStock_query',data = data)
    #设置下载
    tsui::run_download_xlsx(id = 'btnprdInStock_dl',data = data,filename = file_name)




  })

    var_fileprdInStock_upload <- tsui::var_file('fileprdInStock_upload')

  shiny::observeEvent(input$btnprdInStock_upload,{
    file_name = var_fileprdInStock_upload()
    print(file_name)
    if(is.null(file_name)){
      tsui::pop_notice('请选择一个回料粉碎记录表EXCEL文件')
    }else{
      data = prdInStock_read(file_name = file_name,token = token)
      #设置显示
      tsui::run_dataTable2(id = 'dataviewprdInStock_query',data = data)
      tsui::pop_notice('上传服务器成功！')
    }






  })

  shiny::observeEvent(input$btnprdInStock_overwrite,{
    file_name = var_fileprdInStock_upload()
    print(file_name)
    if(is.null(file_name)){
      tsui::pop_notice('请选择一个回料粉碎记录表EXCEL文件')
    }else{
      data = prdInStock_read(file_name = file_name,token = token,overWrite = TRUE)
      #设置显示
      tsui::run_dataTable2(id = 'dataviewprdInStock_query',data = data)
      tsui::pop_notice('上传服务器成功！')
    }






  })






}
