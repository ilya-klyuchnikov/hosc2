package hosc.lang.core

import hosc.lang.hs._

object CoreSource {
	
	def coreModuleFromFile(path: String): Module = {
		val hsModule = HsSource.readHsModule(path)
		val map = HsSource.consInfo(hsModule)
		
		val globalContext = 
			hsModule.binds map {_.v}
		val newBindsBodies = 
			hsModule.binds map {b => DeBruijnConverter.removeNames(map, globalContext, (Nil, Nil), Nil, b.expr)}
		val newBinds = 
			newBindsBodies.zipWithIndex.map {case (nb, i) => Bind(GVar(i), nb)}
		val dataTypes = 
			hsModule.dataDefs map {dt => DataType(dt.cons map {case HsDataCon(name, args) => DataCon(name, args.size)})}
		
		Module(dataTypes, newBinds)
	}
}