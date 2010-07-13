package hosc.lang.core

import hosc.lang.io._

object CoreIn {
	
	def coreModuleFromFile(path: String): Module = {
		val hsModule = HsIn.readHsModule(path)
		val map = HsIn.consInfo(hsModule)
		
		val globalContext = 
			hsModule.binds map {_.v}
		val newBindsBodies = 
			hsModule.binds map {b => DeBruijnConverter.removeNames(map, globalContext, Nil, Nil, b.expr)}
		val newBinds = 
			newBindsBodies.zipWithIndex.map {case (nb, i) => Bind(GVar(i), nb)}
		val dataTypes = 
			hsModule.dataDefs map {dt => DataType(dt.cons map {case HsDataCon(name, args) => DataCon(name, args.size)})}
		
		Module(dataTypes, newBinds)
	}
}