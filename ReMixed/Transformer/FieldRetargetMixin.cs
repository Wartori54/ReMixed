using System.Collections.Generic;
using System.Linq;
using Mono.Cecil;
using Mono.Collections.Generic;
using ReMixed.Processor;

namespace ReMixed.Transformer;

public class FieldRetargetMixin : ITransformer<FieldDefinition, FieldDefinition> {
    public sealed class Factory(RelinkerConfig config) : ITransformerFactory<FieldDefinition, FieldDefinition> {
        public int Pass => -1;
        public IEnumerable<FieldDefinition>? AppliesTo(FieldDefinition memberDef, Collection<FieldDefinition> targets) {
            return targets.Where(t => t.Name == memberDef.Name);
        }
        public ITransformer<FieldDefinition, FieldDefinition> For(FieldDefinition patch, FieldDefinition target) {
            return new FieldRetargetMixin(config);
        }
    }
    
    private readonly RelinkerConfig config;

    private FieldRetargetMixin(RelinkerConfig config) {
        this.config = config;
    }

    public void Perform(FieldDefinition memberDefSource, FieldDefinition memberDefTarget) {
        config.Moved(memberDefSource, memberDefTarget);
    }
}